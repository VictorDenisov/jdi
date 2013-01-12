-- This module is an implementation of JDWP.
-- http://docs.oracle.com/javase/7/docs/platform/jpda/jdwp/jdwp-protocol.html
module Language.Java.Jdwp where

import Data.Binary (Binary(..), Get, Put)
import Data.Binary.Get (runGet, getByteString)
import Data.Binary.Put (runPut, putByteString)

import Data.Bits ((.&.))
import Data.Char (ord)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (find)
import Data.Word (Word8, Word16, Word32, Word64)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard, when)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.IO.Class (MonadIO)
import GHC.IO.Handle ( Handle, hClose, hSetBinaryMode
                     , hPutStr, hFlush, hWaitForInput )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map as M

------------Packet description and parsing section. {{{
type PacketId = Word32
type CommandSet = Word8
type Command = Word8

data Packet = CommandPacket { packetLen  :: Word32
                            , packetId   :: PacketId
                            , flags      :: Word8
                            , commandSet :: CommandSet
                            , command    :: Command
                            , dat        :: B.ByteString
                            }
            | ReplyPacket   { packetLen  :: Word32
                            , packetId   :: PacketId
                            , flags      :: Word8
                            , errorCode  :: Word16
                            , dat        :: B.ByteString
                            }
              deriving Show

packetHeaderSize = 11

parsePacket :: Get Packet
parsePacket = do
    len   <- get
    id    <- get
    flags <- get
    if (flags .&. 0x80) == 0
        then do
            cmdSet <- get
            cmd    <- get
            dat    <- getByteString (fromIntegral len - packetHeaderSize)
            return (CommandPacket len id flags cmdSet cmd dat)
        else do
            err <- get
            dat <- getByteString (fromIntegral len - packetHeaderSize)
            return (ReplyPacket len id flags err dat)

putPacket :: Packet -> Put
putPacket (CommandPacket len id flags cmdSet cmd dat) = do
    put len
    put id
    put flags
    put cmdSet
    put cmd
    putByteString dat

putPacket (ReplyPacket len id flags err dat) = do
    put len
    put id
    put flags
    put err
    putByteString dat
-- }}}

------------General types section {{{
type JavaByte            = Word8
type JavaInt             = Word32
type JavaLong            = Word64
type JavaString          = String
type JavaBoolean         = Bool
type JavaThreadId        = JavaObjectId
type JavaThreadGroupId   = JavaObjectId
type JavaClassId         = JavaReferenceTypeId
data JavaFieldId         = JavaFieldId JavaInt Word64 deriving (Show, Eq)
data JavaMethodId        = JavaMethodId JavaInt Word64 deriving (Show, Eq)
data JavaObjectId        = JavaObjectId JavaInt Word64 deriving (Show, Eq)
type JavaReferenceTypeId = JavaObjectId
data JavaFrameId         = JavaFrameId JavaInt Word64 deriving (Show, Eq)
type JavaInterfaceId     = JavaReferenceTypeId

data JavaLocation = JavaLocation
                  { typeTag :: TypeTag
                  , classId :: JavaClassId
                  , methodId :: JavaMethodId
                  , index    :: Word64
                  } deriving (Show, Eq)

-- Byte marshalling functions
putByte :: JavaByte -> Put
putByte v = put v

parseByte :: Get JavaByte
parseByte = get

-- Boolean marshalling functions
parseBoolean :: Get JavaBoolean
parseBoolean = (/= 0) <$> (get :: Get Word8)

-- Int marshalling functions
putInt :: JavaInt -> Put
putInt v = put v

parseInt :: Get JavaInt
parseInt = get

-- Long marshalling functions
parseLong :: Get JavaLong
parseLong = get

-- String marshalling functions
parseString :: Get JavaString
parseString = do
    len <- fromIntegral <$> (get :: Get Word32)
    B8.unpack <$> getByteString len

putString :: JavaString -> Put
putString s = do
    put $ ((fromIntegral (length s)) :: Word32)
    mapM_ put (B.unpack $ B8.pack $ s)

-- Location marshalling functions
putLocation :: JavaLocation -> Put
putLocation (JavaLocation typeTag classId methodId index) = do
    putTypeTag  typeTag
    putClassId classId
    putMethodId methodId
    put index

parseLocation :: IdSizes -> Get JavaLocation
parseLocation is = JavaLocation <$> parseTypeTag
                                <*> parseClassId (referenceTypeIdSize is)
                                <*> parseMethodId (methodIdSize is)
                                <*> parseLong

-- Dynamic value marshalling functions
putDynamicSizedValue :: JavaInt -> Word64 -> Put
putDynamicSizedValue s v = case s of
    1 -> put ((fromIntegral v) :: Word8)
    2 -> put ((fromIntegral v) :: Word16)
    4 -> put ((fromIntegral v) :: Word32)
    8 -> put ((fromIntegral v) :: Word64)
    _ -> error $ "Currently we can not process values of this size: " ++ show s

parseDynamicSizedValue :: JavaInt -> Get Word64
parseDynamicSizedValue 1 = fromIntegral <$> (get :: Get Word8)
parseDynamicSizedValue 2 = fromIntegral <$> (get :: Get Word16)
parseDynamicSizedValue 4 = fromIntegral <$> (get :: Get Word32)
parseDynamicSizedValue 8 = fromIntegral <$> (get :: Get Word64)
parseDynamicSizedValue s =
    error $ "Currently we can not process values of this size: " ++ show s

--- Field id marshalling funcitons
putFieldId :: JavaFieldId -> Put
putFieldId (JavaFieldId size v) = putDynamicSizedValue size v

parseFieldId :: JavaInt -> Get JavaFieldId
parseFieldId s = JavaFieldId s <$> parseDynamicSizedValue s

--- Method id marshalling funcitons
putMethodId :: JavaMethodId -> Put
putMethodId (JavaMethodId size v) = putDynamicSizedValue size v

parseMethodId :: JavaInt -> Get JavaMethodId
parseMethodId s = JavaMethodId s <$> parseDynamicSizedValue s

--- Object id marshalling funcitons
putObjectId :: JavaObjectId -> Put
putObjectId (JavaObjectId size v) = putDynamicSizedValue size v

parseObjectId :: JavaInt -> Get JavaObjectId
parseObjectId s = JavaObjectId s <$> parseDynamicSizedValue s

--- ReferenceType id marshalling funcitons
putReferenceTypeId :: JavaReferenceTypeId -> Put
putReferenceTypeId (JavaObjectId size v) = putDynamicSizedValue size v

parseReferenceTypeId :: JavaInt -> Get JavaReferenceTypeId
parseReferenceTypeId s = JavaObjectId s <$> parseDynamicSizedValue s

--- Frame id marshalling funcitons
putFrameId :: JavaFrameId -> Put
putFrameId (JavaFrameId size v) = putDynamicSizedValue size v

parseFrameId :: JavaInt -> Get JavaFrameId
parseFrameId s = JavaFrameId s <$> parseDynamicSizedValue s

-- Thread id marshalling funcitons
putThreadId :: JavaThreadId -> Put
putThreadId = putObjectId

parseThreadId :: JavaInt -> Get JavaThreadId
parseThreadId = parseObjectId

-- Thread group id marshalling funcitons
putThreadGroupId :: JavaThreadGroupId -> Put
putThreadGroupId = putObjectId

parseThreadGroupId :: JavaInt -> Get JavaThreadGroupId
parseThreadGroupId = parseObjectId

-- Class id marshalling funcitons
putClassId :: JavaClassId -> Put
putClassId = putReferenceTypeId

parseClassId :: JavaInt -> Get JavaClassId
parseClassId = parseReferenceTypeId

data Version = Version
                    { description :: JavaString
                    , jdwpMajor   :: JavaInt
                    , jdwpMinor   :: JavaInt
                    , vmVersion   :: JavaString
                    , vmName      :: JavaString
                    } deriving (Eq, Show)

data IdSizes = IdSizes
                    { fieldIdSize         :: JavaInt
                    , methodIdSize        :: JavaInt
                    , objectIdSize        :: JavaInt
                    , referenceTypeIdSize :: JavaInt
                    , frameIdSize         :: JavaInt
                    } deriving (Eq, Show)

threadIdSize :: IdSizes -> JavaInt
threadIdSize is = objectIdSize is

threadGroupIdSize :: IdSizes -> JavaInt
threadGroupIdSize is = objectIdSize is

{- | Several Event objects may be created at a given time by the target
VirtualMachine. For example, there may be more than one BreakpointRequest
for a given Location or you might single step to the same location as
a BreakpointRequest. These Event objects are delivered together as
an EventSet. For uniformity, an EventSet is always used to deliver Event
objects. EventSets are delivered by the EventQueue.
EventSets are unmodifiable.

Associated with the issuance of an event set, suspensions may have occurred in
the target VM. These suspensions correspond with the suspend policy. To assure
matching resumes occur, it is recommended, where possible, to complete the
processing of an event set with EventSet.resume().

The events that are grouped in an EventSet are restricted in the following ways:

Always singleton sets:

    - VMStartEvent

    - VMDisconnectEvent

Only with other VMDeathEvents:

    - VMDeathEvent

Only with other ThreadStartEvents for the same thread:

    - ThreadStartEvent

Only with other ThreadDeathEvents for the same thread:

    - ThreadDeathEvent

Only with other ClassPrepareEvents for the same class:

    - ClassPrepareEvent

Only with other ClassUnloadEvents for the same class:

    - ClassUnloadEvent

Only with other AccessWatchpointEvents for the same field access:

    - AccessWatchpointEvent

Only with other ModificationWatchpointEvents for the same field modification:

    - ModificationWatchpointEvent

Only with other ExceptionEvents for the same exception occurrance:

    - ExceptionEvent

Only with other MethodExitEvents for the same method exit:

    - MethodExitEvent

Only with other members of this group, at the same location
and in the same thread:

    - BreakpointEvent

    - StepEvent

    - MethodEntryEvent
-}
data EventSet = EventSet
              { suspendPolicy :: SuspendPolicy
                -- | In essence this function is eventIterator from JDI.
              , events        :: [Event]
              } deriving (Show, Eq)

-- requestId, event kind specific data
data Event = VmStartEvent
                    JavaInt
                    JavaThreadId
           | VmDeathEvent
                    JavaInt
           | ClassPrepareEvent
                    JavaInt
                    JavaThreadId
                    TypeTag
                    JavaReferenceTypeId
                    JavaString -- signature
                    ClassStatus
           | BreakpointEvent
                    JavaInt
                    JavaThreadId
                    JavaLocation
           | StepEvent
                    JavaInt
                    JavaThreadId
                    JavaLocation
           | NoEvent
             deriving (Show, Eq)

threadId :: Event -> JavaThreadId
threadId (VmStartEvent _ ti) = ti
threadId (ClassPrepareEvent _ ti _ _ _ _) = ti
threadId (BreakpointEvent _ ti _) = ti
threadId (StepEvent _ ti _) = ti

-- | Returns type of event.
eventKind :: Event -> EventKind
eventKind (VmStartEvent {})= VmStart
eventKind (VmDeathEvent {})= VmDeath
eventKind (ClassPrepareEvent {})= ClassPrepare
eventKind (BreakpointEvent {})= Breakpoint
eventKind (StepEvent {})= SingleStep

-- | Types of events can be received from the virtual machine.
data EventKind = VmDisconnected
               | VmStart
               | ThreadDeath
               | SingleStep
               | Breakpoint
               | FramePop
               | Exception
               | UserDefined
               | ThreadStart
               | ThreadEnd
               | ClassPrepare
               | ClassUnload
               | ClassLoad
               | FieldAccess
               | FieldModification
               | ExceptionCatch
               | MethodEntry
               | MethodExit
               | VmInit
               | VmDeath
                 deriving (Eq, Show)

data SuspendPolicy = SuspendNone
                   | SuspendEventThread
                   | SuspendAll
                     deriving (Eq, Show)

data EventModifier = Count JavaInt
                   | Conditional JavaInt -- exprId
                   | ThreadOnly JavaThreadId
                   | ClassOnly JavaReferenceTypeId -- clazz
                   | ClassMatch JavaString -- classPattern
                   | ClassExclude JavaString -- classPattern
                   | LocationOnly JavaLocation
                                 --exceptionOrNull     caught      uncaught
                   | ExceptionOnly JavaReferenceTypeId JavaBoolean JavaBoolean
                            -- declaring           fieldId
                   | FieldOnly JavaReferenceTypeId JavaFieldId
                   | Step JavaThreadId StepSize StepDepth
                               -- instance
                   | InstanceOnly JavaObjectId
                     deriving (Show, Eq)

data TypeTag = Class
             | Interface
             | Array
               deriving (Eq, Show)

data Tag = ArrayTag
         | ByteTag
         | CharTag
         | ObjectTag
         | FloatTag
         | DoubleTag
         | IntTag
         | LongTag
         | ShortTag
         | VoidTag
         | BooleanTag
         | StringTag
         | ThreadTag
         | ThreadGroupTag
         | ClassLoaderTag
         | ClassObjectTag
           deriving (Eq, Show)

newtype ClassStatus = ClassStatus JavaInt
                      deriving (Eq, Show)

data ThreadStatus = Zombie
                  | Running
                  | Sleeping
                  | Monitor
                  | Wait
                    deriving (Eq, Show)

data SuspendStatus = Resumed
                   | Suspended
                     deriving (Eq, Show)

data ReferenceType = ReferenceType
                            TypeTag
                            JavaReferenceTypeId
                            JavaString
                            ClassStatus
                     deriving (Eq, Show)

data ObjectReference = ObjectReference JavaObjectId
                       deriving (Eq, Show)

{- | Provides access to an array object and its components in the target VM.
Each array component is mirrored by a Value object. The array components, in
aggregate, are placed in List objects instead of arrays for consistency with
the rest of the API and for interoperability with other APIs.
-}
data ArrayReference = ArrayReference JavaObjectId
                      deriving (Eq, Show)

data StringReference = StringReference JavaObjectId
                      deriving (Eq, Show)

                    -- fieldId name signature modBits
data Field = Field JavaFieldId String String JavaInt
             deriving (Eq, Show)

field_public       = 0x0001 :: JavaInt
field_private      = 0x0002 :: JavaInt
field_protected    = 0x0004 :: JavaInt
field_static       = 0x0008 :: JavaInt
field_final        = 0x0010 :: JavaInt
field_volatile     = 0x0040 :: JavaInt
field_transient    = 0x0080 :: JavaInt
field_synthetic    = 0x1000 :: JavaInt
field_enum         = 0x4000 :: JavaInt

                    -- methodId name signature modBits
data Method = Method JavaMethodId String String JavaInt
              deriving (Eq, Show)

method_public       = 0x0001 :: JavaInt
method_private      = 0x0002 :: JavaInt
method_protected    = 0x0004 :: JavaInt
method_static       = 0x0008 :: JavaInt
method_final        = 0x0010 :: JavaInt
method_synchronized = 0x0020 :: JavaInt
method_bridge       = 0x0040 :: JavaInt
method_varargs      = 0x0080 :: JavaInt
method_native       = 0x0100 :: JavaInt
method_abstract     = 0x0400 :: JavaInt
method_strict       = 0x0800 :: JavaInt
method_synthetic    = 0x1000 :: JavaInt

data LineTable = LineTable JavaLong JavaLong [Line] -- start, end, lines
                 deriving (Eq, Show)

data VariableTable = VariableTable JavaInt [Slot] -- arg count, slots
                     deriving (Eq, Show)

data Line = Line JavaLong JavaInt -- codeIndex number
            deriving (Eq, Show)

data Slot = Slot JavaLong String String JavaInt JavaInt
            deriving (Eq, Show)
              -- codeIndex, name, signature, length, slot

data StepSize = StepMin
              | StepLine
                deriving (Eq, Show)

data StepDepth = StepInto
               | StepOut
               | StepOver
                 deriving (Eq, Show)

data StackFrame = StackFrame JavaFrameId JavaLocation
                  deriving (Eq, Show)

data Value = ArrayValue JavaObjectId
           | ByteValue Int8
           | CharValue Word16
           | ObjectValue JavaObjectId
           | FloatValue Float
           | DoubleValue Double
           | IntValue Int32
           | LongValue Int64
           | ShortValue Int16
           | VoidValue
           | BooleanValue Word8
           | StringValue JavaObjectId
           | ThreadValue JavaObjectId
           | ThreadGroupValue JavaObjectId
           | ClassLoaderValue JavaObjectId
           | ClassObjectValue JavaObjectId
             deriving (Eq, Show)

data Capabilities = Capabilities
    { canWatchFieldModification        :: JavaBoolean
    , canWatchFieldAccess              :: JavaBoolean
    , canGetBytecodes                  :: JavaBoolean
    , canGetSynteticAttribute          :: JavaBoolean
    , canGetOwnedMonitorInfo           :: JavaBoolean
    , canGetCurrentContendedMonitor    :: JavaBoolean
    , canGetMonitorInfo                :: JavaBoolean
    , canRedefineClasses               :: JavaBoolean
    , canAddMethod                     :: JavaBoolean
    , canUnrestrictedlyRedefineClasses :: JavaBoolean
    , canPopFrames                     :: JavaBoolean
    , canUseInstanceFilters            :: JavaBoolean
    , canGetSourceDebugExtension       :: JavaBoolean
    , canRequestVmDeathEvent           :: JavaBoolean
    , canSetDefaultStratum             :: JavaBoolean
    } deriving (Show, Eq)

lengthOfJavaString :: JavaString -> Word32
lengthOfJavaString s = 4 + (fromIntegral $ length s)

lengthOfEventModifier :: EventModifier -> Word32
lengthOfEventModifier (Count _) = 1 + 4
lengthOfEventModifier (Conditional _) = 1 + 4
lengthOfEventModifier (ThreadOnly (JavaObjectId l v)) = 1 + l
lengthOfEventModifier (ClassOnly (JavaObjectId l v)) = 1 + l
lengthOfEventModifier (ClassMatch s) = 1 + 4 + fromIntegral (length s)
lengthOfEventModifier (ClassExclude s) = 1 + 4 + fromIntegral (length s)
lengthOfEventModifier (LocationOnly
                            (JavaLocation _
                                          (JavaObjectId refSize _)
                                          (JavaMethodId mSize _)
                                          _
                            )
                      ) = 1 + 1 + refSize + mSize + 8
lengthOfEventModifier (Step (JavaObjectId l v) _ _) = 1 + l + 4 + 4
lengthOfEventModifier _ = error "unhandled size yet"

fromNumber :: (Eq n, Show n) => [(n, a)] -> n -> a
fromNumber list n =
    case find ((== n) . fst) list of
        Just (_, v)  -> v
        Nothing ->
            error $ "Number " ++ show n ++ " doesn't match any value from list"

toNumber :: (Eq a, Show a) => [(n, a)] -> a -> n
toNumber list e =
    case find ((== e) . snd) list of
        Just (n, _) -> n
        Nothing     -> error $ "list doesn't have value " ++ show e

threadStatusNumbers :: [(JavaInt, ThreadStatus)]
threadStatusNumbers = [ (0, Zombie)
                      , (1, Running)
                      , (2, Sleeping)
                      , (3, Monitor)
                      , (4, Wait)
                      ]

suspendStatusNumbers :: [(JavaInt, SuspendStatus)]
suspendStatusNumbers = [ (0, Resumed)
                       , (1, Suspended)
                       ]

eventKindNumbers :: [(JavaByte, EventKind)]
eventKindNumbers = [ (  1, SingleStep)
                   , (  2, Breakpoint)
                   , (  3, FramePop)
                   , (  4, Exception)
                   , (  5, UserDefined)
                   , (  6, ThreadStart)
                   , (  7, ThreadEnd)
                   , (  8, ClassPrepare)
                   , (  9, ClassUnload)
                   , ( 10, ClassLoad)
                   , ( 20, FieldAccess)
                   , ( 21, FieldModification)
                   , ( 30, ExceptionCatch)
                   , ( 40, MethodEntry)
                   , ( 41, MethodExit)
                   , ( 90, VmInit)
                   , ( 99, VmDeath)
                   , (100, VmDisconnected)
                   ]

suspendPolicyNumbers :: [(JavaByte, SuspendPolicy)]
suspendPolicyNumbers = [ (0, SuspendNone)
                       , (1, SuspendEventThread)
                       , (2, SuspendAll)
                       ]

typeTagNumbers :: [(JavaByte, TypeTag)]
typeTagNumbers = [ (1, Class)
                 , (2, Interface)
                 , (3, Array)
                 ]

tagNumbers :: [(JavaByte, Tag)]
tagNumbers = [ (91 , ArrayTag)
             , (66 , ByteTag)
             , (67 , CharTag)
             , (76 , ObjectTag)
             , (70 , FloatTag)
             , (68 , DoubleTag)
             , (73 , IntTag)
             , (74 , LongTag)
             , (83 , ShortTag)
             , (86 , VoidTag)
             , (90 , BooleanTag)
             , (115, StringTag)
             , (116, ThreadTag)
             , (103, ThreadGroupTag)
             , (108, ClassLoaderTag)
             , (99 , ClassObjectTag)
             ]

primitiveTag :: Tag -> Bool
primitiveTag tg = case tg of
                    ByteTag -> True
                    BooleanTag -> True
                    CharTag -> True
                    DoubleTag -> True
                    FloatTag -> True
                    IntTag -> True
                    LongTag -> True
                    ShortTag -> True
                    otherwise -> False

errorList = [ ( 10, "Passed thread is null, is not a valid thread or has exited.")
            , ( 11, "Thread group invalid.")
            , ( 12, "Invalid priority.")
            , ( 13, "If the specified thread has not been suspended by an event.")
            , ( 14, "Thread already suspended.")
            , ( 20, "The reference type has been unloaded or garbage collected.")
            , ( 21, "Invalid class")
            , ( 22, "Class has been loaded, but not yet prepared.")
            , ( 23, "Invalid method.")
            , ( 24, "Invalid location.")
            , ( 25, "Invalid field.")
            , ( 30, "Invalid jframeID.")
            , ( 31, "There are no more Java or JNI frames on the call stack.")
            , ( 32, "Information about the frame is not available.")
            , ( 33, "Operation can only be performed on current frame.")
            , ( 34, "The variable is not an appropriate type for the function used.")
            , ( 35, "Invalid slot.")
            , ( 40, "Item already set.")
            , ( 41, "Desired element not found.")
            , ( 50, "Invalid monitor.")
            , ( 51, "This thread doesn't own the monitor.")
            , ( 52, "The call has been interrupted before completion.")
            , ( 60, "The call has been interrupted before completion.")
            , ( 61, "A circularity has been detected while initializing a class.")
            , ( 62, "The verifier detected that a class file, though well formed, contained some sort of internal inconsistency or security problem.")
            , ( 63, "Adding methods has not been implemented.")
            , ( 64, "Schema change has not been implemented.")
            , ( 65, "The state of the thread has been modified, and is now inconsistent.")
            , ( 66, "A direct superclass is different for the new class version, or the set of directly implemented interfaces is different and canUnrestrictedlyRedefineClasses is false.")
            , ( 67, "The new class version does not declare a method declared in the old class version and canUnrestrictedlyRedefineClasses is false.")
            , ( 68, "A class file has a version number not supported by this VM.")
            , ( 69, "The class name defined in the new class file is different from the name in the old class object.")
            , ( 70, "The new class version has different modifiers and and canUnrestrictedlyRedefineClasses is false.")
            , ( 71, "A method in the new class version has different modifiers than its counterpart in the old class version and and canUnrestrictedlyRedefineClasses is false.")
            , ( 99, "The functionality is not implemented in this virtual machine.")
            , (100, "Invalid pointer.")
            , (101, "Desired information is not available.")
            , (102, "The specified event type id is not recognized.")
            , (103, "Illegal argument.")
            , (110, "The function needed to allocate memory and no more memory was available for allocation.")
            , (111, "Debugging has not been enabled in this virtual machine. JVMDI cannot be used.")
            , (112, "The virtual machine is not running.")
            , (113, "An unexpected internal error has occurred.")
            , (115, "The thread being used to call this function is not attached to the virtual machine. Calls must be made from attached threads.")
            , (500, "Invalid object type id or class tag.")
            , (502, "Previous invoke not complete.")
            , (503, "Index is invalid.")
            , (504, "The length is invalid.")
            , (506, "The string is invalid.")
            , (507, "The class loader is invalid.")
            , (508, "The array is invalid.")
            , (509, "Unable to load the transport.")
            , (510, "Unable to initialize the transport.")
            , (511, "Native method.")
            , (512, "The count is invalid.")
            ]

errorFromCode :: Int -> String
errorFromCode n = snd $ head $ filter ((n ==) . fst) errorList

--- SuspendPolicy
putSuspendPolicy :: SuspendPolicy -> Put
putSuspendPolicy s = put $ (toNumber suspendPolicyNumbers) s

parseSuspendPolicy :: Get SuspendPolicy
parseSuspendPolicy = (fromNumber suspendPolicyNumbers) <$> (get :: Get JavaByte)

--- ClassStatus
putClassStatus :: ClassStatus -> Put
putClassStatus (ClassStatus v) = put v

parseClassStatus :: Get ClassStatus
parseClassStatus = ClassStatus <$> get

--- EventKind
putEventKind :: EventKind -> Put
putEventKind e = put $ (toNumber eventKindNumbers) e

parseEventKind :: Get EventKind
parseEventKind = (fromNumber eventKindNumbers) <$> (get :: Get JavaByte)

--- TypeTag
putTypeTag :: TypeTag -> Put
putTypeTag t = put $ (toNumber typeTagNumbers) t

parseTypeTag :: Get TypeTag
parseTypeTag = (fromNumber typeTagNumbers) <$> (get :: Get JavaByte)

--- Tag
parseTag :: Get Tag
parseTag = (fromNumber tagNumbers) <$> (get :: Get JavaByte)

--- StepSize
putStepSize :: StepSize -> Put
putStepSize StepMin  = putInt 0
putStepSize StepLine = putInt 1

--- StepDepth
putStepDepth :: StepDepth -> Put
putStepDepth StepInto = putInt 0
putStepDepth StepOver = putInt 1
putStepDepth StepOut  = putInt 2

--- EventModifier
putEventModifier :: EventModifier -> Put
putEventModifier (Count count) =
    putByte 1 >> put count
putEventModifier (Conditional exprId) =
    putByte 2 >> put exprId
putEventModifier (ThreadOnly threadId) =
    putByte 3 >> putThreadId threadId
putEventModifier (ClassOnly clazz) =
    putByte 4 >> putReferenceTypeId clazz
putEventModifier (ClassMatch classPattern) =
    putByte 5 >> putString classPattern
putEventModifier (ClassExclude classPattern) =
    putByte 6 >> putString classPattern
putEventModifier (LocationOnly location) =
    putByte 7 >> putLocation location
putEventModifier (ExceptionOnly exceptionOrNull caught uncaught) = do
    putByte 8
    putReferenceTypeId exceptionOrNull
    put caught
    put uncaught
putEventModifier (FieldOnly declaring fieldId) = do
    putByte 9
    putReferenceTypeId declaring
    putFieldId fieldId
putEventModifier (Step threadId size depth) = do
    putByte 10
    putThreadId threadId
    putStepSize size
    putStepDepth depth
putEventModifier (InstanceOnly inst) = do
    putByte 11
    putObjectId inst

parseIdSizes :: Get IdSizes
parseIdSizes = IdSizes <$> parseInt
                       <*> parseInt
                       <*> parseInt
                       <*> parseInt
                       <*> parseInt

parseVersion :: Get Version
parseVersion = Version <$> parseString
                       <*> parseInt
                       <*> parseInt
                       <*> parseString
                       <*> parseString

parseEventSet :: IdSizes -> Get EventSet
parseEventSet idsizes = do
    sp <- parseSuspendPolicy
    eventCount <- parseInt
    eventList <- mapM (\_ -> parseEvent idsizes) [1..eventCount]
    return $ EventSet sp eventList

parseEvent :: IdSizes -> Get Event
parseEvent idsizes = do
    eventKind <- parseEventKind
    case eventKind of
        ClassPrepare ->
            ClassPrepareEvent
                <$> parseInt
                <*> (parseThreadId $ threadIdSize idsizes)
                <*> parseTypeTag
                <*> (parseReferenceTypeId $ referenceTypeIdSize idsizes)
                <*> parseString
                <*> parseClassStatus
        Breakpoint -> BreakpointEvent
                            <$> parseInt
                            <*> parseThreadId (threadIdSize idsizes)
                            <*> parseLocation idsizes
        SingleStep -> StepEvent
                            <$> parseInt
                            <*> parseThreadId (threadIdSize idsizes)
                            <*> parseLocation idsizes
        VmInit  -> VmStartEvent
                            <$> parseInt
                            <*> (parseThreadId $ threadIdSize idsizes)
        VmDeath -> VmDeathEvent
                            <$> parseInt
        _       -> return NoEvent

parseReferenceType :: IdSizes -> Get ReferenceType
parseReferenceType idsizes =
    ReferenceType
        <$> parseTypeTag
        <*> (parseReferenceTypeId $ referenceTypeIdSize idsizes)
        <*> parseString
        <*> parseClassStatus

parseReferenceTypeNoSignature :: IdSizes -> Get ReferenceType
parseReferenceTypeNoSignature idsizes =
    ReferenceType
        <$> parseTypeTag
        <*> (parseReferenceTypeId $ referenceTypeIdSize idsizes)
        <*> (return "")
        <*> parseClassStatus

parseField :: IdSizes -> Get Field
parseField idsizes =
    Field
        <$> (parseFieldId $ fieldIdSize idsizes)
        <*> parseString
        <*> parseString
        <*> parseInt

parseFieldsReply :: IdSizes -> Get [Field]
parseFieldsReply idsizes = do
    fieldCount <- parseInt
    mapM (\_ -> parseField idsizes) [1..fieldCount]

parseMethod :: IdSizes -> Get Method
parseMethod idsizes =
    Method
        <$> (parseMethodId $ methodIdSize idsizes)
        <*> parseString
        <*> parseString
        <*> parseInt

parseMethodsReply :: IdSizes -> Get [Method]
parseMethodsReply idsizes = do
    methodCount <- parseInt
    mapM (\_ -> parseMethod idsizes) [1..methodCount]

parseAllClassesReply :: IdSizes -> Get [ReferenceType]
parseAllClassesReply idsizes = do
    classCount <- parseInt
    mapM (\_ -> parseReferenceType idsizes) [1..classCount]

parseClassesBySignatureReply :: IdSizes -> Get [ReferenceType]
parseClassesBySignatureReply idsizes = do
    classCount <- parseInt
    mapM (\_ -> parseReferenceTypeNoSignature idsizes) [1..classCount]

parseInterfacesReply :: IdSizes -> Get [JavaInterfaceId]
parseInterfacesReply idsizes = do
    interfaceCount <- parseInt
    mapM (\_ -> parseReferenceTypeId $ referenceTypeIdSize idsizes) [1..interfaceCount]

parseAllThreadsReply :: IdSizes -> Get [JavaThreadId]
parseAllThreadsReply idsizes = do
    threadCount <- parseInt
    mapM (\_ -> parseThreadId $ threadIdSize idsizes) [1..threadCount]

parseThreadGroupsReply :: IdSizes -> Get [JavaThreadGroupId]
parseThreadGroupsReply idsizes = do
    groupCount <- parseInt
    mapM (\_ -> parseThreadGroupId $ threadGroupIdSize idsizes) [1..groupCount]

parseThreadGroupChildrenReply :: IdSizes
                              -> Get ( [JavaThreadId]
                                     , [JavaThreadGroupId])
parseThreadGroupChildrenReply idsizes = do
    thread <- parseAllThreadsReply idsizes
    threadGroups <- parseThreadGroupsReply idsizes
    return (thread, threadGroups)

parseThreadStatusReply :: Get (ThreadStatus, SuspendStatus)
parseThreadStatusReply = do
    ts <- parseInt
    ss <- parseInt
    let threadStatus = fromNumber threadStatusNumbers ts
    let suspendStatus = fromNumber suspendStatusNumbers ss
    return (threadStatus, suspendStatus)

parseLineTableReply :: Get LineTable
parseLineTableReply = do
    start <- parseLong
    end   <- parseLong
    lineCount <- parseInt
    lines <- mapM (\_ -> parseLine) [1..lineCount]
    return $ LineTable start end lines

parseMonitorInfoReply :: IdSizes
                      -> Get (JavaThreadId, JavaInt, [JavaThreadId])
parseMonitorInfoReply idsizes = do
    owner <- parseThreadId $ threadIdSize idsizes
    entryCount <- parseInt
    waiters <- parseAllThreadsReply idsizes
    return (owner, entryCount, waiters)

parseLine :: Get Line
parseLine = Line <$> parseLong <*> parseInt

parseVariableTableReply :: Get VariableTable
parseVariableTableReply = do
    argCnt <- parseInt
    slotCount <- parseInt
    slots <- mapM (\_ -> parseSlot) [1..slotCount]
    return $ VariableTable argCnt slots

parseSlot :: Get Slot
parseSlot = Slot
              <$> parseLong
              <*> parseString
              <*> parseString
              <*> parseInt
              <*> parseInt

parseGetValuesReply :: IdSizes -> Get [Value]
parseGetValuesReply idsizes = do
    valuesCount <- parseInt
    mapM (\_ -> parseTaggedValue idsizes) [1..valuesCount]

parseTaggedValue :: IdSizes -> Get Value
parseTaggedValue idsizes = do
    tg <- parseTag
    parseUntaggedValue idsizes tg

parseUntaggedValue :: IdSizes -> Tag -> Get Value
parseUntaggedValue idsizes tg =
    case tg of
        ArrayTag -> ArrayValue <$> parseObjectId (objectIdSize idsizes)
        ByteTag -> ByteValue <$> (get :: Get Int8)
        CharTag -> CharValue <$> (get :: Get Word16)
        ObjectTag -> ObjectValue <$> parseObjectId (objectIdSize idsizes)
        FloatTag -> FloatValue <$> (get :: Get Float)
        DoubleTag -> DoubleValue <$> (get :: Get Double)
        IntTag -> IntValue <$> (get :: Get Int32)
        LongTag -> LongValue <$> (get :: Get Int64)
        ShortTag -> ShortValue <$> (get :: Get Int16)
        VoidTag -> return VoidValue
        BooleanTag -> BooleanValue <$> (get :: Get Word8)
        StringTag -> StringValue <$> parseObjectId (objectIdSize idsizes)
        ThreadTag -> ThreadValue <$> parseObjectId (objectIdSize idsizes)
        ThreadGroupTag -> ThreadGroupValue
                                <$> parseObjectId (objectIdSize idsizes)
        ClassLoaderTag -> ClassLoaderValue
                                <$> parseObjectId (objectIdSize idsizes)
        ClassObjectTag -> ClassObjectValue
                                <$> parseObjectId (objectIdSize idsizes)

parseArrayRegion :: IdSizes -> Get [Value]
parseArrayRegion idsizes = do
    tg <- parseTag
    valuesCount <- parseInt
    if primitiveTag tg
        then mapM (\_ -> parseUntaggedValue idsizes tg) [1..valuesCount]
        else mapM (\_ -> parseTaggedValue idsizes)      [1..valuesCount]

putEventRequest :: EventKind -> SuspendPolicy -> [EventModifier] -> Put
putEventRequest ek sp ems = do
    putEventKind ek
    putSuspendPolicy sp
    put ((fromIntegral $ length ems)  :: JavaInt)
    mapM_ putEventModifier ems

putClearEvent :: EventKind -> JavaInt -> Put
putClearEvent ek requestId = do
    putEventKind ek
    put requestId

parseStackFrame :: IdSizes -> Get StackFrame
parseStackFrame idsizes = do
    StackFrame
        <$> parseFrameId (frameIdSize idsizes)
        <*> parseLocation idsizes

parseStackFrameList :: IdSizes -> Get [StackFrame]
parseStackFrameList idsizes = do
    frameCount <- parseInt
    mapM (\_ -> parseStackFrame idsizes) [1..frameCount]

toStrict :: LB.ByteString -> B.ByteString
toStrict = B.concat . LB.toChunks

toLazy :: B.ByteString -> LB.ByteString
toLazy v = LB.fromChunks [v]
-- }}}

------------Command Constructors Section {{{
-- Commands are ordered accordingly to their group and id numbers.

-- Virtual Machine command set. (1) {{{
versionCommand :: PacketId -> Packet
versionCommand packetId = CommandPacket 11 packetId 0 1 1 B.empty

classesBySignatureCommand :: JavaString -> PacketId -> Packet
classesBySignatureCommand jniName packetId =
    CommandPacket
        (11 + lengthOfJavaString jniName)
        packetId
        0 1 2
        (toStrict $ runPut $ putString jniName)

allClassesCommand :: PacketId -> Packet
allClassesCommand packetId = CommandPacket 11 packetId 0 1 3 B.empty

allThreadsCommand :: PacketId -> Packet
allThreadsCommand packetId = CommandPacket 11 packetId 0 1 4 B.empty

topLevelThreadGroupsCommand :: PacketId -> Packet
topLevelThreadGroupsCommand packetId = CommandPacket 11 packetId 0 1 5 B.empty

disposeCommand :: PacketId -> Packet
disposeCommand packetId = CommandPacket 11 packetId 0 1 6 B.empty

idSizesCommand :: PacketId -> Packet
idSizesCommand packetId = CommandPacket 11 packetId 0 1 7 B.empty

suspendVmCommand :: PacketId -> Packet
suspendVmCommand packetId = CommandPacket 11 packetId 0 1 8 B.empty

resumeVmCommand :: PacketId -> Packet
resumeVmCommand packetId = CommandPacket 11 packetId 0 1 9 B.empty

exitCommand :: JavaInt -> PacketId -> Packet
exitCommand packetId exitCode =
    CommandPacket (11 + 4) packetId 0 1 10 (toStrict $ runPut $ put exitCode)

capabilitiesCommand :: PacketId -> Packet
capabilitiesCommand packetId = CommandPacket 11 packetId 0 1 12 B.empty

classPathsCommand :: PacketId -> Packet
classPathsCommand packetId = CommandPacket 11 packetId 0 1 13 B.empty

capabilitiesNewCommand :: PacketId -> Packet
capabilitiesNewCommand packetId = CommandPacket 11 packetId 0 1 17 B.empty
--}}}

-- ReferenceType command set. (2) {{{
signatureCommand :: JavaReferenceTypeId -> PacketId -> Packet
signatureCommand typeId@(JavaObjectId rSize _) packetId =
    CommandPacket
        (11 + rSize)
        packetId 0 2 1
        (toStrict $ runPut $ putReferenceTypeId typeId)

classLoaderCommand :: JavaReferenceTypeId -> PacketId -> Packet
classLoaderCommand typeId@(JavaObjectId rSize _) packetId =
    CommandPacket
        (11 + rSize)
        packetId 0 2 2
        (toStrict $ runPut $ putReferenceTypeId typeId)

modifiersCommand :: JavaReferenceTypeId -> PacketId -> Packet
modifiersCommand typeId@(JavaObjectId rSize _) packetId =
    CommandPacket
        (11 + rSize)
        packetId 0 2 3
        (toStrict $ runPut $ putReferenceTypeId typeId)

fieldsCommand :: JavaReferenceTypeId -> PacketId -> Packet
fieldsCommand typeId@(JavaObjectId rSize _) packetId =
    CommandPacket
        (11 + rSize)
        packetId 0 2 4
        (toStrict $ runPut $ putReferenceTypeId typeId)

methodsCommand :: JavaReferenceTypeId -> PacketId -> Packet
methodsCommand typeId@(JavaObjectId size _) packetId =
    CommandPacket
        (11 + size)
        packetId 0 2 5
        (toStrict $ runPut $ putReferenceTypeId typeId)

refGetValuesCommand :: JavaReferenceTypeId -> [Field] -> PacketId -> Packet
refGetValuesCommand typeId@(JavaObjectId size _) fs packetId =
    CommandPacket
        (11 + size + 4 + len fs * fieldSize fs)
        packetId 0 2 6
        (toStrict $ runPut $ do
            putReferenceTypeId typeId
            putInt $ len fs
            putFields fs)
    where
        len = fromIntegral . length
        fieldSize [] = 0
        fieldSize ((Field (JavaFieldId fsize _ ) _ _ _) : _) = fsize
        putFields = mapM_ $ \(Field id _ _ _) -> putFieldId id

sourceFileCommand :: JavaReferenceTypeId -> PacketId -> Packet
sourceFileCommand
                typeId@(JavaObjectId rSize _)
                packetId =
    CommandPacket
        (11 + rSize)
        packetId 0 2 7
        (toStrict $ runPut $ putReferenceTypeId typeId)

statusCommand :: JavaReferenceTypeId -> PacketId -> Packet
statusCommand typeId@(JavaObjectId rSize _) packetId =
    CommandPacket
        (11 + rSize)
        packetId 0 2 9
        (toStrict $ runPut $ putReferenceTypeId typeId)

interfacesCommand :: JavaReferenceTypeId -> PacketId -> Packet
interfacesCommand
        typeId@(JavaObjectId rSize _)
        packetId =
    CommandPacket
        (11 + rSize)
        packetId 0 2 10
        (toStrict $ runPut $ putReferenceTypeId typeId)

-- }}}

-- ClassType command set. (3) {{{

superclassCommand :: JavaReferenceTypeId -> PacketId -> Packet
superclassCommand
        typeId@(JavaObjectId rSize _)
        packetId =
    CommandPacket
        (11 + rSize)
        packetId 0 3 1
        (toStrict $ runPut $ putReferenceTypeId typeId)

-- }}}

-- Method command set. (6) {{{
lineTableCommand :: JavaReferenceTypeId -> JavaMethodId -> PacketId -> Packet
lineTableCommand
            typeId@(JavaObjectId rSize _)
            methodId@(JavaMethodId mSize _)
            packetId =
    CommandPacket
            (11 + rSize + mSize)
            packetId 0 6 1
            (toStrict $ runPut $ putReferenceTypeId typeId
                                 >> putMethodId methodId)

variableTableCommand :: JavaReferenceTypeId
                     -> JavaMethodId
                     -> PacketId
                     -> Packet
variableTableCommand refId@(JavaObjectId rSize _)
                     mId@(JavaMethodId mSize _)
                     packetId =
    CommandPacket
        (11 + rSize + mSize)
        packetId 0 6 2
        (toStrict $ runPut $ (putReferenceTypeId refId >> putMethodId mId))
-- }}}

-- ObjectReference command set. (9) {{{
objGetValuesCommand :: JavaObjectId -> [Field] -> PacketId -> Packet
objGetValuesCommand typeId@(JavaObjectId size _) fs packetId =
    CommandPacket
        (11 + size + 4 + len fs * fieldSize fs)
        packetId 0 9 2
        (toStrict $ runPut $ do
            putReferenceTypeId typeId
            putInt $ len fs
            putFields fs)
    where
        len = fromIntegral . length
        fieldSize [] = 0
        fieldSize ((Field (JavaFieldId fsize _ ) _ _ _) : _) = fsize
        putFields = mapM_ $ \(Field id _ _ _) -> putFieldId id

monitorInfoCommand :: JavaObjectId -> PacketId -> Packet
monitorInfoCommand oId@(JavaObjectId s _) packetId =
    CommandPacket
        (11 + s)
        packetId 0 9 5
        (toStrict $ runPut $ putObjectId oId)

disableCollectionCommand :: JavaObjectId -> PacketId -> Packet
disableCollectionCommand oId@(JavaObjectId s _) packetId =
    CommandPacket
        (11 + s)
        packetId 0 9 7
        (toStrict $ runPut $ putObjectId oId)

enableCollectionCommand :: JavaObjectId -> PacketId -> Packet
enableCollectionCommand oId@(JavaObjectId s _) packetId =
    CommandPacket
        (11 + s)
        packetId 0 9 8
        (toStrict $ runPut $ putObjectId oId)
-- }}}

-- StringReference command set. (10) {{{
stringValueCommand :: JavaObjectId -> PacketId -> Packet
stringValueCommand sId@(JavaObjectId s _) packetId =
    CommandPacket
        (11 + s)
        packetId 0 10 1
        (toStrict $ runPut $ putObjectId sId)
-- }}}

-- ThreadReference command set. (11) {{{
threadReferenceNameCommand :: JavaThreadId -> PacketId -> Packet
threadReferenceNameCommand ti@(JavaObjectId size _) packetId =
    CommandPacket
        (11 + size)
        packetId 0 11 1
        (toStrict $ runPut $ putThreadId ti)

resumeThreadCommand :: JavaThreadId -> PacketId -> Packet
resumeThreadCommand threadId packetId =
    CommandPacket
        19
        packetId
        0 11 3
        (toStrict $ runPut $ putThreadId threadId)

threadStatusCommand :: JavaThreadId -> PacketId -> Packet
threadStatusCommand threadId@(JavaObjectId s _) packetId =
    CommandPacket
        (11 + s)
        packetId
        0 11 4
        (toStrict $ runPut $ putThreadId threadId)

threadGroupCommand :: JavaThreadId -> PacketId -> Packet
threadGroupCommand threadId@(JavaObjectId s _) packetId =
    CommandPacket
        (11 + s)
        packetId
        0 11 5
        (toStrict $ runPut $ putThreadId threadId)

framesCommand :: JavaThreadId -> JavaInt -> JavaInt -> PacketId -> Packet
framesCommand threadId@(JavaObjectId s _) startFrame len packetId =
    CommandPacket
        (11 + s + 4 + 4)
        packetId 0 11 6
        (toStrict $ runPut $ do
                            putThreadId threadId
                            putInt startFrame
                            putInt len)

frameCountCommand :: JavaThreadId -> PacketId -> Packet
frameCountCommand threadId@(JavaObjectId s _) packetId =
    CommandPacket
        (11 + s)
        packetId 0 11 7
        (toStrict $ runPut $ putThreadId threadId)
-- }}}

-- ThreadGroupReference command set. (12) {{{

threadGroupReferenceNameCommand tgi@(JavaObjectId size _) packetId =
    CommandPacket
        (11 + size)
        packetId 0 12 1
        (toStrict $ runPut $ putThreadGroupId tgi)

threadGroupReferenceParentCommand tgi@(JavaObjectId size _) packetId =
    CommandPacket
        (11 + size)
        packetId 0 12 2
        (toStrict $ runPut $ putThreadGroupId tgi)

threadGroupReferenceChildrenCommand tgi@(JavaObjectId size _) packetId =
    CommandPacket
        (11 + size)
        packetId 0 12 3
        (toStrict $ runPut $ putThreadGroupId tgi)

-- }}}

-- ArrayReference command set. (13) {{{
lengthCommand :: JavaObjectId -> PacketId -> Packet
lengthCommand
            arrayId@(JavaObjectId st _)
            packetId =
    CommandPacket
        (11 + st)
        packetId 0 13 1
        (toStrict $ runPut $ putObjectId arrayId)

getArrayValuesCommand :: JavaObjectId
                      -> JavaInt
                      -> JavaInt
                      -> PacketId
                      -> Packet
getArrayValuesCommand arrayId@(JavaObjectId st _) firstIndex length packetId =
    CommandPacket
        (11 + st + 4 + 4)
        packetId 0 13 2
        (toStrict $ runPut $ do
                            putObjectId arrayId
                            putInt firstIndex
                            putInt length)

-- }}}

-- EventRequest command set. (15) {{{
eventSetRequest :: EventKind
                -> SuspendPolicy
                -> [EventModifier]
                -> PacketId
                -> Packet
eventSetRequest ek sp ems packetId =
    CommandPacket
        (11 + (6 + lengthOfEventModifiers))
        packetId
        0 15 1
        (toStrict $ runPut $ putEventRequest ek sp ems)
    where
        lengthOfEventModifiers = (foldr (+) 0 (map lengthOfEventModifier ems))

eventClearRequest :: EventKind -> JavaInt -> PacketId -> Packet
eventClearRequest ek requestId packetId =
    CommandPacket
        (11 + 5)
        packetId
        0 15 2
        (toStrict $ runPut $ putClearEvent ek requestId)
-- }}}

-- StackFrame command set. (16) {{{
getValuesCommand :: JavaThreadId -> JavaFrameId -> [Slot] -> PacketId -> Packet
getValuesCommand
            threadId@(JavaObjectId st _)
            frameId@(JavaFrameId sf _)
            slots
            packetId =
    CommandPacket
        (11 + st + sf + 4 + (4 + 1) * (fromIntegral $ length slots))
        packetId 0 16 1
        (toStrict $ runPut $ do
                            putThreadId threadId
                            putFrameId  frameId
                            putInt (fromIntegral $ length slots)
                            putSlots slots)
    where
        putSlots = mapM_ $ \(Slot _ _ sig _ slot) -> do
            putInt slot
            putByte $ fromIntegral $ ord $ head sig

thisObjectCommand :: JavaThreadId -> JavaFrameId -> PacketId -> Packet
thisObjectCommand
            threadId@(JavaObjectId st _)
            frameId@(JavaFrameId sf _)
            packetId =
    CommandPacket
        (11 + st + sf)
        packetId 0 16 3
        (toStrict $ runPut $ do
                            putThreadId threadId
                            putFrameId  frameId)
-- }}}
-- }}}

------------Jdwp communication functions {{{

receivePacket :: Handle -> IO Packet
receivePacket h = do
    inputAvailable <- hWaitForInput h (-1)
    if inputAvailable
        then do lengthString <- LB.hGet h 4
                let l = (fromIntegral $ runGet (parseInt) lengthString) - 4
                reminder <- LB.hGet h l
                let p = runGet parsePacket (lengthString `LB.append` reminder)
                return p
        else error "No input available where expected"

waitReply :: Handle -> IO Packet
waitReply h = do
    packet <- receivePacket h
    case packet of
        CommandPacket _ _ _ _ _ _
            -> error "reply expected, but command received"
        {- Normally here some queue should be implemented, but currectly
         - for brevity we assume that we never get event before reply.
         -}
        ReplyPacket _ _ _ _ _ -> return packet

waitEvent :: Handle -> IO Packet
waitEvent h = do
    packet <- receivePacket h
    case packet of
        CommandPacket _ _ _ _ _ _ -> return packet
        ReplyPacket _ _ _ _ _
            -> error "CommandPacket is expected, but reply packet received"

sendPacket :: Handle -> Packet -> IO ()
sendPacket h p = do
    LB.hPut h $ runPut $ putPacket p
    hFlush h

-- }}}
-- vim: foldmethod=marker foldmarker={{{,}}}

module Jdwp where

import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map as M
import Data.Binary (Binary(..), Get, Put)
import Data.Bits ((.&.))
import Data.List (find)
import Control.Applicative ((<$>), (<*>))
import Data.Binary.Get (runGet, getByteString)
import Data.Binary.Put (runPut)
import Control.Monad.Trans (liftIO, lift)
import GHC.IO.Handle (Handle, hClose, hSetBinaryMode, hPutStr, hFlush, hWaitForInput)
import Control.Monad (guard, when)
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.IO.Class (MonadIO)

------------Packet description and parsing section.
-- {{{
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
    l <- get
    i <- get
    f <- get
    if (f .&. 0x80) == 0
        then do
            cs <- get
            c  <- get
            d  <- getByteString (fromIntegral l - packetHeaderSize)
            return (CommandPacket l i f cs c d)
        else do
            e <- get
            d <- getByteString (fromIntegral l - packetHeaderSize)
            return (ReplyPacket l i f e d)

putPacket :: Packet -> Put
putPacket (CommandPacket l i f cs c d) = do
    put l
    put i
    put f
    put cs
    put c

putPacket (ReplyPacket l i f e d) = do
    put l
    put i
    put f
    put e
-- }}}
---------------General types section
-- {{{
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
data JavaReferenceTypeId = JavaReferenceTypeId JavaInt Word64 deriving (Show, Eq) -- size value
data JavaFrameId         = JavaFrameId JavaInt Word64 deriving (Show, Eq)

data JavaLocation = JavaLocation
                  { typeTag :: TypeTag
                  , classId :: JavaClassId
                  , methodId :: JavaMethodId
                  , index    :: Word64
                  } deriving (Show, Eq)

putByte :: JavaByte -> Put
putByte v = put v

parseByte :: Get JavaByte
parseByte = get

parseBoolean :: Get JavaBoolean
parseBoolean = (/= 0) <$> (get :: Get Word8)

parseInt :: Get JavaInt
parseInt = get

parseLong :: Get JavaLong
parseLong = get

parseString :: Get JavaString
parseString = do
    len <- fromIntegral <$> (get :: Get Word32)
    B8.unpack <$> getByteString len

putString :: JavaString -> Put
putString s = do
    put $ ((fromIntegral (length s)) :: Word32)
    mapM_ put (B.unpack $ B8.pack $ s)

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

putDynamicSizedValue :: JavaInt -> Word64 -> Put
putDynamicSizedValue s v = case s of
    1 -> put ((fromIntegral v) :: Word8)
    2 -> put ((fromIntegral v) :: Word16)
    4 -> put ((fromIntegral v) :: Word32)
    8 -> put ((fromIntegral v) :: Word64)
    _ -> error $ "Currently we can not process values of this size: " ++ (show s)

parseDynamicSizedValue :: JavaInt -> Get Word64
parseDynamicSizedValue 1 = fromIntegral <$> (get :: Get Word8)
parseDynamicSizedValue 2 = fromIntegral <$> (get :: Get Word16)
parseDynamicSizedValue 4 = fromIntegral <$> (get :: Get Word32)
parseDynamicSizedValue 8 = fromIntegral <$> (get :: Get Word64)
parseDynamicSizedValue s = error $ "Currently we can not process values of this size: " ++ (show s)

--- fieldId
putFieldId :: JavaFieldId -> Put
putFieldId (JavaFieldId size v) = putDynamicSizedValue size v

parseFieldId :: JavaInt -> Get JavaFieldId
parseFieldId s = JavaFieldId s <$> parseDynamicSizedValue s

--- methodId
putMethodId :: JavaMethodId -> Put
putMethodId (JavaMethodId size v) = putDynamicSizedValue size v

parseMethodId :: JavaInt -> Get JavaMethodId
parseMethodId s = JavaMethodId s <$> parseDynamicSizedValue s

--- objectId
putObjectId :: JavaObjectId -> Put
putObjectId (JavaObjectId size v) = putDynamicSizedValue size v

parseObjectId :: JavaInt -> Get JavaObjectId
parseObjectId s = JavaObjectId s <$> parseDynamicSizedValue s

--- referenceTypeId
putReferenceTypeId :: JavaReferenceTypeId -> Put
putReferenceTypeId (JavaReferenceTypeId size v) = putDynamicSizedValue size v

parseReferenceTypeId :: JavaInt -> Get JavaReferenceTypeId
parseReferenceTypeId s = JavaReferenceTypeId s <$> parseDynamicSizedValue s

--- frameId
putFrameId :: JavaFrameId -> Put
putFrameId (JavaFrameId size v) = putDynamicSizedValue size v

parseFrameId :: JavaInt -> Get JavaFrameId
parseFrameId s = JavaFrameId s <$> parseDynamicSizedValue s

----------
putThreadId :: JavaThreadId -> Put
putThreadId = putObjectId

parseThreadId :: JavaInt -> Get JavaThreadId
parseThreadId = parseObjectId

putThreadGroupId :: JavaThreadGroupId -> Put
putThreadGroupId = putObjectId

parseThreadGroupId :: JavaInt -> Get JavaThreadGroupId
parseThreadGroupId = parseObjectId

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

data EventSet = EventSet
              { suspendPolicy :: SuspendPolicy
                -- | In essence this function is eventIterator from JDI.
              , events        :: [Event]
              } deriving (Show, Eq)

--                event kind, requestId, event kind specific data
data Event = Event EventKind JavaInt Event
           | VmStartEvent
                    JavaThreadId
           | VmDeathEvent
           | ClassPrepareEvent
                    JavaThreadId
                    TypeTag
                    JavaReferenceTypeId
                    JavaString
                    ClassStatus
           | BreakpointEvent
                    JavaThreadId
                    JavaLocation
           | NoEvent
             deriving (Show, Eq)

eventKind :: Event -> EventKind
eventKind (Event ek _ _) = ek

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
                       -- threadId     size    depth
                   | Step JavaThreadId JavaInt JavaInt
                               -- instance
                   | InstanceOnly JavaObjectId
                     deriving (Show, Eq)

data TypeTag = Class
             | Interface
             | Array
               deriving (Eq, Show)

newtype ClassStatus = ClassStatus JavaInt
                      deriving (Eq, Show)

data ReferenceType = ReferenceType TypeTag JavaReferenceTypeId JavaString ClassStatus
                     deriving (Eq, Show)

data Method = Method JavaMethodId String String JavaInt -- methodId name signature modBits
              deriving (Eq, Show)

data ThreadReference = ThreadReference JavaThreadId
                       deriving (Eq, Show)

data ThreadGroupReference = ThreadGroupReference JavaThreadGroupId
                            deriving (Eq, Show)

data LineTable = LineTable JavaLong JavaLong [Line] -- start end lines
                 deriving (Eq, Show)

data Line = Line JavaLong JavaInt -- codeIndex number
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
lengthOfEventModifier (ClassOnly (JavaReferenceTypeId l v)) = 1 + l
lengthOfEventModifier (ClassMatch s) = 1 + 4 + fromIntegral (length s)
lengthOfEventModifier (ClassExclude s) = 1 + 4 + fromIntegral (length s)
lengthOfEventModifier (LocationOnly
                            (JavaLocation _
                                          (JavaReferenceTypeId refSize _)
                                          (JavaMethodId mSize _)
                                          _
                            )
                      ) = 1 + 1 + refSize + mSize + 8
lengthOfEventModifier _ = error "unhandled size yet"

fromNumber :: [(JavaByte, a)] -> JavaByte -> a
fromNumber list n = case find ((== n) . fst) list of
                            Just (_, v)  -> v
                            Nothing -> error $ "Number " ++ (show n) ++ " doesn't match any value from list"

toNumber :: (Eq a, Show a) => [(JavaByte, a)] -> a -> JavaByte
toNumber list e = case find ((== e) . snd) list of
                            Just (n, _) -> n
                            Nothing     -> error $ "list doesn't have value " ++ (show e)

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

--- EventModifier
putEventModifier :: EventModifier -> Put
putEventModifier (Count count)               = putByte 1 >> put count
putEventModifier (Conditional exprId)        = putByte 2 >> put exprId
putEventModifier (ThreadOnly threadId)       = putByte 3 >> putThreadId threadId
putEventModifier (ClassOnly clazz)           = putByte 4 >> putReferenceTypeId clazz
putEventModifier (ClassMatch classPattern)   = putByte 5 >> putString classPattern
putEventModifier (ClassExclude classPattern) = putByte 6 >> putString classPattern
putEventModifier (LocationOnly location)     = putByte 7 >> putLocation location
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
    put size
    put depth
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
    Event eventKind <$> parseInt <*>
        case eventKind of
            ClassPrepare -> ClassPrepareEvent
                                <$> (parseThreadId $ threadIdSize idsizes)
                                <*> parseTypeTag
                                <*> (parseReferenceTypeId $ referenceTypeIdSize idsizes)
                                <*> parseString
                                <*> parseClassStatus
            Breakpoint -> BreakpointEvent
                                <$> parseThreadId (threadIdSize idsizes)
                                <*> parseLocation idsizes
            VmInit  -> VmStartEvent <$> (parseThreadId $ threadIdSize idsizes)
            VmDeath -> return VmDeathEvent
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

parseThreadReference :: IdSizes -> Get ThreadReference
parseThreadReference idsizes = ThreadReference <$> (parseThreadId $ threadIdSize idsizes)

parseAllThreadsReply :: IdSizes -> Get [ThreadReference]
parseAllThreadsReply idsizes = do
    threadCount <- parseInt
    mapM (\_ -> parseThreadReference idsizes) [1..threadCount]

parseThreadGroupReference :: IdSizes -> Get ThreadGroupReference
parseThreadGroupReference idsizes =
    ThreadGroupReference <$> (parseThreadGroupId $ threadGroupIdSize idsizes)

parseThreadGroupsReply :: IdSizes -> Get [ThreadGroupReference]
parseThreadGroupsReply idsizes = do
    groupCount <- parseInt
    mapM (\_ -> parseThreadGroupReference idsizes) [1..groupCount]

parseLineTableReply :: Get LineTable
parseLineTableReply = do
    start <- parseLong
    end   <- parseLong
    lineCount <- parseInt
    lines <- mapM (\_ -> parseLine) [1..lineCount]
    return $ LineTable start end lines

parseLine :: Get Line
parseLine = Line <$> parseLong <*> parseInt

putEventRequest :: EventKind -> SuspendPolicy -> [EventModifier] -> Put
putEventRequest ek sp ems = do
    putEventKind ek
    putSuspendPolicy sp
    put ((fromIntegral $ length ems)  :: JavaInt)
    mapM_ putEventModifier ems

toStrict :: LB.ByteString -> B.ByteString
toStrict = B.concat . LB.toChunks

toLazy :: B.ByteString -> LB.ByteString
toLazy v = LB.fromChunks [v]
-- }}}
------------Command Constructors Section
-- {{{
versionCommand :: PacketId -> Packet
versionCommand packetId = CommandPacket 11 packetId 0 1 1 B.empty

idSizesCommand :: PacketId -> Packet
idSizesCommand packetId = CommandPacket 11 packetId 0 1 7 B.empty

resumeVmCommand :: PacketId -> Packet
resumeVmCommand packetId = CommandPacket 11 packetId 0 1 9 B.empty

resumeThreadCommand :: PacketId -> JavaThreadId -> Packet
resumeThreadCommand packetId threadId = CommandPacket 19 packetId 0 11 3 (toStrict $ runPut $ putThreadId threadId)

eventSetRequest :: PacketId -> EventKind -> SuspendPolicy -> [EventModifier] -> Packet
eventSetRequest packetId ek sp ems = CommandPacket
                                        (11 + (6 + lengthOfEventModifiers))
                                        packetId
                                        0
                                        15
                                        1
                                        (toStrict $ runPut $ putEventRequest ek sp ems)
                            where lengthOfEventModifiers = (foldr (+) 0 (map lengthOfEventModifier ems))


capabilitiesCommand :: PacketId -> Packet
capabilitiesCommand packetId = CommandPacket 11 packetId 0 1 12 B.empty

capabilitiesNewCommand :: PacketId -> Packet
capabilitiesNewCommand packetId = CommandPacket 11 packetId 0 1 17 B.empty

allClassesCommand :: PacketId -> Packet
allClassesCommand packetId = CommandPacket 11 packetId 0 1 3 B.empty

allThreadsCommand :: PacketId -> Packet
allThreadsCommand packetId = CommandPacket 11 packetId 0 1 4 B.empty

classesBySignatureCommand :: PacketId -> JavaString -> Packet
classesBySignatureCommand packetId jniName =
    CommandPacket
        (11 + lengthOfJavaString jniName)
        packetId
        0
        1
        2
        (toStrict $ runPut $ putString jniName)

exitCommand :: PacketId -> JavaInt -> Packet
exitCommand packetId exitCode =
    CommandPacket (11 + 4) packetId 0 1 10 (toStrict $ runPut $ put exitCode)

topLevelThreadGroupsCommand :: PacketId -> Packet
topLevelThreadGroupsCommand packetId = CommandPacket 11 packetId 0 1 5 B.empty

disposeCommand :: PacketId -> Packet
disposeCommand packetId = CommandPacket 11 packetId 0 1 6 B.empty

methodsCommand :: PacketId -> JavaReferenceTypeId -> Packet
methodsCommand packetId typeId@(JavaReferenceTypeId size _) =
    CommandPacket
        (11 + size)
        packetId 0 2 5
        (toStrict $ runPut $ putReferenceTypeId typeId)

threadReferenceNameCommand :: PacketId -> JavaThreadId -> Packet
threadReferenceNameCommand packetId ti@(JavaObjectId size _) =
    CommandPacket
        (11 + size)
        packetId 0 11 1
        (toStrict $ runPut $ putThreadId ti)

lineTableCommand :: PacketId -> JavaReferenceTypeId -> JavaMethodId -> Packet
lineTableCommand
            packetId
            typeId@(JavaReferenceTypeId rSize _)
            methodId@(JavaMethodId mSize _)
    = CommandPacket
            (11 + rSize + mSize)
            packetId 0 6 1
            (toStrict $ runPut $ putReferenceTypeId typeId
                                 >> putMethodId methodId)

-- }}}
------------Jdwp communication functions
-- {{{

handshake :: MonadIO m => Handle -> ErrorT String m ()
handshake h = do
    liftIO $ putStrLn "Connected. Initiating handshake..."
    liftIO $ hPutStr h "JDWP-Handshake"
    liftIO $ hFlush h
    value <- liftIO $ B.hGet h 14
    when (value /= (B8.pack "JDWP-Handshake")) $ fail "Handshake FAILED."
    liftIO $ putStrLn "Handshake successful."

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
        CommandPacket _ _ _ _ _ _ -> error "reply expected, but command received"
        {- Normally here some queue should be implemented, but currectly for brevity
         - we assume that we never get event before reply.
         -}
        ReplyPacket _ _ _ _ _ -> return packet

waitEvent :: Handle -> IO Packet
waitEvent h = do
    packet <- receivePacket h
    case packet of
        CommandPacket _ _ _ _ _ _ -> return packet
        ReplyPacket _ _ _ _ _ -> error "CommandPacket is expected, but reply packet received"

sendPacket :: Handle -> Packet -> IO ()
sendPacket h p = do
    LB.hPut h $ runPut $ putPacket p
    B.hPut h $ dat p
    hFlush h

-- }}}
-- vim: foldmethod=marker foldmarker={{{,}}}

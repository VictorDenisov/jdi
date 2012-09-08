module Jdwp where

import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map as M
import Data.Binary (Binary(..), Get, Put)
import Data.Bits ((.&.))
import Data.List (find)
import Control.Applicative ((<$>), (<*>))
import Data.Binary.Get (runGet)
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
                            , dat        :: PacketData
                            }
            | ReplyPacket   { packetLen  :: Word32
                            , packetId   :: PacketId
                            , flags      :: Word8
                            , errorCode  :: Word16
                            , dat        :: PacketData
                            }
              deriving Show

type ReplyDataParser = PacketId -> Get PacketData

parsePacket :: IdSizes -> ReplyDataParser -> Get Packet
parsePacket idsizes replyDataParser = do
    l <- get
    i <- get
    f <- get
    if (f .&. 0x80) == 0
    then do
        cs <- get
        c  <- get
        d  <- (commandParser $ dataParsers (cs, c)) idsizes
        return (CommandPacket l i f cs c d)
    else do
        e <- get
        d <- case e of
            0 -> replyDataParser i
            _ -> parseEmptyData idsizes
        return (ReplyPacket l i f e d)

parseList :: Word32 -> Get a -> Get [a]
parseList 0 p = return []
parseList l p = do
    x <- p
    xs <- parseList (l - 1) p
    return (x:xs)

putPacket :: Packet -> Put
putPacket (CommandPacket l i f cs c d) = do
    put l
    put i
    put f
    put cs
    put c
    putPacketData d

putPacket (ReplyPacket l i f e d) = do
    put l
    put i
    put f
    put e
    putPacketData d
-- }}}
---------------General types section
-- {{{
type JavaByte            = Word8
type JavaInt             = Word32
type JavaLong            = Word64
type JavaString          = String
type JavaBoolean         = Bool
type JavaThreadId        = JavaObjectId
type JavaClassId         = JavaReferenceTypeId
data JavaFieldId      = JavaFieldId JavaInt Word64 deriving (Show, Eq)
data JavaMethodId     = JavaMethodId JavaInt Word64 deriving (Show, Eq)
data JavaObjectId     = JavaObjectId JavaInt Word64 deriving (Show, Eq)
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
    len <- (get :: Get Word32)
    list <- parseList len (get :: Get Word8)
    return $ B8.unpack $ B.pack list

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

putClassId :: JavaClassId -> Put
putClassId = putReferenceTypeId

parseClassId :: JavaInt -> Get JavaClassId
parseClassId = parseReferenceTypeId
-- }}}
-------PacketData parsing section---------------------
-- {{{
-- PacketData components declaration.
---- {{{
data PacketData = EventSetData EventSet
                | VersionReply 
                    { description :: JavaString
                    , jdwpMajor   :: JavaInt
                    , jdwpMinor   :: JavaInt
                    , vmVersion   :: JavaString
                    , vmName      :: JavaString
                    }
                | IdSizesReply
                    { idSizes :: IdSizes
                    }
                | ThreadIdPacketData
                    { tId :: JavaThreadId
                    }
                | EventRequestSetPacketData EventKind SuspendPolicy [EventModifier]
                | EventRequestSetReply
                    { requestIdReply :: JavaInt
                    }
                | EmptyPacketData
                  deriving (Eq, Show)

data IdSizes = IdSizes
                    { fieldIdSize         :: JavaInt
                    , methodIdSize        :: JavaInt
                    , objectIdSize        :: JavaInt
                    , referenceTypeIdSize :: JavaInt
                    , frameIdSize         :: JavaInt
                    } deriving (Eq, Show)

data EventSet = EventSet
              { suspendPolicy :: SuspendPolicy
              , events        :: [Event]
              } deriving (Show, Eq)

threadIdSize :: IdSizes -> JavaInt
threadIdSize is = objectIdSize is

data Event = VmStartEvent
                { requestId :: JavaInt
                , threadId  :: JavaThreadId
                }
           | VmDeathEvent
                { requestId :: JavaInt
                }
           | ClassPrepareEvent
                { requestId   :: JavaInt
                , threadId    :: JavaThreadId
                , refTypeTag  :: TypeTag
                , typeId      :: JavaReferenceTypeId
                , signature   :: JavaString
                , classStatus :: ClassStatus
                }
           | NoEvent
             deriving (Show, Eq)

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

data SuspendPolicy = SuspenNone
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

fromNumber :: [(JavaByte, a)] -> JavaByte -> a
fromNumber list n = case find ((== n) . fst) list of
                            Just (_, v)  -> v
                            Nothing -> error $ "Number " ++ (show n) ++ " doesn't match any value from list"

toNumber :: (Eq a, Show a) => [(JavaByte, a)] -> a -> JavaByte
toNumber list e = case find ((== e) . snd) list of
                            Just (n, _) -> n
                            Nothing     -> error $ "list doesn't have value " ++ (show e)

eventKindNumberList :: [(JavaByte, EventKind)]
eventKindNumberList = [ (  1, SingleStep)
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

eventKindFromNumber :: JavaByte -> EventKind
eventKindFromNumber = fromNumber eventKindNumberList

numberFromEventKind :: EventKind -> JavaByte
numberFromEventKind = toNumber eventKindNumberList

suspendPolicyNumberList :: [(JavaByte, SuspendPolicy)]
suspendPolicyNumberList = [ (0, SuspenNone)
                          , (1, SuspendEventThread)
                          , (2, SuspendAll)
                          ]

suspendPolicyFromNumber :: JavaByte -> SuspendPolicy
suspendPolicyFromNumber = fromNumber suspendPolicyNumberList

numberFromSuspendPolicy :: SuspendPolicy -> JavaByte
numberFromSuspendPolicy = toNumber suspendPolicyNumberList

typeTagNumberList :: [(JavaByte, TypeTag)]
typeTagNumberList = [ (1, Class)
                    , (2, Interface)
                    , (3, Array)
                    ]

typeTagFromNumber :: JavaByte -> TypeTag
typeTagFromNumber = fromNumber typeTagNumberList

numberFromTypeTag :: TypeTag -> JavaByte
numberFromTypeTag = toNumber typeTagNumberList

-- }}}
-- Parsing and putting functions
---- {{{
--- SuspendPolicy
putSuspendPolicy :: SuspendPolicy -> Put
putSuspendPolicy s = put $ numberFromSuspendPolicy s

parseSuspendPolicy :: Get SuspendPolicy
parseSuspendPolicy = suspendPolicyFromNumber <$> (get :: Get JavaByte)

--- ClassStatus
putClassStatus :: ClassStatus -> Put
putClassStatus (ClassStatus v) = put v

parseClassStatus :: Get ClassStatus
parseClassStatus = ClassStatus <$> get

--- EventKind
putEventKind :: EventKind -> Put
putEventKind e = put $ numberFromEventKind e

parseEventKind :: Get EventKind
parseEventKind = eventKindFromNumber <$> (get :: Get JavaByte)

--- TypeTag
putTypeTag :: TypeTag -> Put
putTypeTag t = put $ numberFromTypeTag t

parseTypeTag :: Get TypeTag
parseTypeTag = typeTagFromNumber <$> (get :: Get JavaByte)

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

putPacketData :: PacketData -> Put
putPacketData (EventSetData (EventSet sp e)) = do
    putSuspendPolicy sp
    mapM_ putEvent e
putPacketData (ThreadIdPacketData i) =
    putThreadId i
putPacketData (EventRequestSetPacketData ek sp ems) = do
    putEventKind ek
    putSuspendPolicy sp
    put ((fromIntegral $ length ems)  :: JavaInt)
    mapM_ putEventModifier ems
putPacketData (EmptyPacketData) = return ()

putEvent :: Event -> Put
putEvent (VmStartEvent ri ti) = do
    put ri
    putThreadId ti

parseIdSizesReply :: IdSizes -> Get PacketData
parseIdSizesReply _ = IdSizesReply <$> (IdSizes
                        <$> parseInt
                        <*> parseInt
                        <*> parseInt
                        <*> parseInt
                        <*> parseInt)

parseVersionReply :: IdSizes -> Get PacketData
parseVersionReply _ = VersionReply
                        <$> parseString
                        <*> parseInt
                        <*> parseInt
                        <*> parseString
                        <*> parseString

parseEventSetRequestReply :: IdSizes -> Get PacketData
parseEventSetRequestReply _ = EventRequestSetReply
                        <$> parseInt

parseEventSet :: IdSizes -> Get PacketData
parseEventSet idsizes = do
    sp <- parseSuspendPolicy
    eventCount <- parseInt
    eventList <- parseList eventCount (parseEvent idsizes)
    return $ EventSetData (EventSet sp eventList)

parseEvent :: IdSizes -> Get Event
parseEvent idsizes = do
    eventKind <- parseEventKind
    case eventKind of
        ClassPrepare -> ClassPrepareEvent
                            <$> parseInt
                            <*> (parseThreadId $ threadIdSize idsizes)
                            <*> parseTypeTag
                            <*> (parseReferenceTypeId $ referenceTypeIdSize idsizes)
                            <*> parseString
                            <*> parseClassStatus
        VmInit  -> VmStartEvent <$> parseInt <*> (parseThreadId $ threadIdSize idsizes)
        VmDeath -> VmDeathEvent <$> parseInt
        _       -> return NoEvent

parseEmptyData :: IdSizes -> Get PacketData
parseEmptyData _ = return EmptyPacketData
-- }}}
data DataParser = DataParser
                { commandParser :: IdSizes -> Get PacketData
                , replyParser   :: IdSizes -> Get PacketData
                }

dataParsers :: (CommandSet, Command) -> DataParser
dataParsers ( 1,   1) = DataParser parseEmptyData parseVersionReply
dataParsers ( 1,   7) = DataParser parseEmptyData parseIdSizesReply
dataParsers (15,   1) = DataParser parseEmptyData parseEventSetRequestReply
dataParsers (64, 100) = DataParser parseEventSet  parseEmptyData
dataParsers _ = undefined

-- }}}
------------Command Constructors Section
-- {{{
versionCommand :: PacketId -> Packet
versionCommand packetId = CommandPacket 11 packetId 0 1 1 EmptyPacketData

idSizesCommand :: PacketId -> Packet
idSizesCommand packetId = CommandPacket 11 packetId 0 1 7 EmptyPacketData

resumeVmCommand :: PacketId -> Packet
resumeVmCommand packetId = CommandPacket 11 packetId 0 1 9 EmptyPacketData

resumeThreadCommand :: PacketId -> JavaThreadId -> Packet
resumeThreadCommand packetId threadId = CommandPacket 19 packetId 0 11 3 (ThreadIdPacketData threadId)

eventSetRequest :: PacketId -> EventKind -> SuspendPolicy -> [EventModifier] -> Packet
eventSetRequest packetId ek sp ems = CommandPacket 17 packetId 0 15 1 $ EventRequestSetPacketData ek sp ems

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

receivePacket :: Handle -> IdSizes -> ReplyDataParser -> IO Packet
receivePacket h idsizes f = do
    inputAvailable <- hWaitForInput h (-1)
    if inputAvailable
    then do lengthString <- B.hGet h 4
            let l = (fromIntegral $ runGet (parseInt) lengthString) - 4
            reminder <- B.hGet h l
            let p = runGet (parsePacket idsizes f) (lengthString `B.append` reminder)
            return p
    else error "No input available where expected"

waitReply :: Handle -> IdSizes -> ReplyDataParser -> IO Packet
waitReply h idsizes f = do
    packet <- receivePacket h idsizes f
    case packet of
        CommandPacket _ _ _ _ _ _ -> error "reply expected, but command received"
        {- Normally here some queue should be implemented, but currectly for brevity
         - we assume that we never get event before reply.
         -}
        ReplyPacket _ _ _ _ _ -> return packet

waitEvent :: Handle -> IdSizes -> IO Packet
waitEvent h idsizes = do
    packet <- receivePacket h idsizes $ \_ -> error "ReplyDataParser is invoked where only command parsing is expected"
    case packet of
        CommandPacket _ _ _ _ _ _ -> return packet
        ReplyPacket _ _ _ _ _ -> error "CommandPacket is expected, but reply packet received"

-- When we parse this event we don't have information about size of threadId.
-- We use the fact that threadId is the last field in the event and we can determine its size
-- as request_length - length_of_fields_before_threadId.
-- for current version of JDWP length_of_fields_before_threadIs is 21.
waitVmStartEvent :: Handle -> IO Packet
waitVmStartEvent h = do
    inputAvailable <- hWaitForInput h (-1)
    if inputAvailable
    then do putStrLn "Waiting for VmStartEvent"
            lengthString <- B.hGet h 4
            let l = (fromIntegral $ runGet (parseInt) lengthString) - 4
            reminder <- B.hGet h l
            let threadIdSize = fromIntegral $ (l + 4) - 21
            let replyParser = \_ -> error "ReplyDataParser is not expected to be invoked."
            let p = runGet (parsePacket (IdSizes 0 0 threadIdSize 0 0) replyParser) (lengthString `B.append` reminder)
            return p
    else error "No input available where expected"

sendPacket :: Handle -> Packet -> IO ()
sendPacket h p = do
    B.hPut h $ runPut $ putPacket p
    hFlush h

-- }}}
-- vim: foldmethod=marker foldmarker={{{,}}}

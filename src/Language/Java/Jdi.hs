module Language.Java.Jdi
( VirtualMachine
, runVirtualMachine
, Name(..)
, Resumable(..)
, Locatable(..)
, SourceName(..)
, AllLineLocations(..)
, vmName
, description
, version
, allClasses
, allThreads
, canAddMethod
, canBeModified
, canGetBytecodes
, canGetCurrentContendedMonitor
, canGetMonitorInfo
, canGetOwnedMonitorInfo
, canGetSourceDebugExtension
, canGetSynteticAttribute
, canPopFrames
, canRedefineClasses
, canRequestVmDeathEvent
, canUnrestrictedlyRedefineClasses
, canUseInstanceFilters
, canWatchFieldAccess
, canWatchFieldModification
, classesByName
, dispose
, exit
, resumeVm
, topLevelThreadGroups
, J.Event
, referenceType
, thread
, J.EventKind(..)
, J.eventKind
, J.EventSet(..)
, removeEvent
, EventRequest
, enable
, disable
, addCountFilter
, createClassPrepareRequest
, createBreakpointRequest
, createStepRequest
, J.ReferenceType
, genericSignature
, allFields
, allMethods
, J.ArrayReference
, getArrValue
, getArrValues
, arrLength
, J.StringReference
, stringValue
, Value(..)
, StackFrame
, getValue
, J.ThreadReference
, allFrames
, frameCount
, frames
, J.ThreadGroupReference
, J.StepSize(..)
, J.StepDepth(..)
, Field
, Method
, arguments
, variables
, variablesByName
, LocalVariable
, Location
, codeIndex
, declaringType
, lineNumber
, method
, J.SuspendPolicy(..)
) where

-- Imports {{{
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import qualified Language.Java.Jdwp as J
import Network (connectTo, PortID)
import qualified Data.Map as M
import Network.Socket.Internal (PortNumber(..))
import GHC.IO.Handle (Handle, hClose, hSetBinaryMode, hPutStr, hFlush)
import Control.Monad.Trans (liftIO, lift, MonadTrans)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>), Applicative(..))
import Control.Monad (liftM, liftM2, ap, guard, when, mapM, void, filterM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.List (find)
import GHC.IO.Handle (hWaitForInput)
import qualified Data.Sequence as S
-- }}}

type VirtualMachine = StateT VmState

-- VmState section {{{
data VmState = VmState
    { idSizesConf     :: Maybe J.IdSizes
    , packetIdCounter :: J.PacketId
    , vmHandle        :: Handle
    , eventQueue      :: S.Seq J.EventSet
    , jdwpVersion     :: Maybe JdwpVersion
    , capabilities    :: Maybe J.Capabilities
    }

data JdwpVersion = JdwpVersion J.JavaInt J.JavaInt deriving (Show, Eq)

instance Ord JdwpVersion where
    compare (JdwpVersion maj1 min1) (JdwpVersion maj2 min2) =
        case min1 `compare` min2 of
            EQ -> maj1 `compare` maj2
            v  -> v

yieldPacketIdCounter :: Monad m => VirtualMachine m J.PacketId
yieldPacketIdCounter = incPacketIdCounter >> getPacketIdCounter

getPacketIdCounter :: Monad m => VirtualMachine m J.PacketId
getPacketIdCounter = packetIdCounter `liftM` get

incPacketIdCounter :: Monad m => VirtualMachine m ()
incPacketIdCounter = do
    s <- get
    put $ s { packetIdCounter = (packetIdCounter s) + 1 }

addToQueue :: Monad m => J.EventSet -> VirtualMachine m ()
addToQueue e = do
    s <- get
    put $ s { eventQueue = (eventQueue s) S.|> e}

takeFromQueue :: Monad m => VirtualMachine m J.EventSet
takeFromQueue = do
    s <- get
    let e = (S.index (eventQueue s) 0)
    put $ s { eventQueue = (S.drop 1 (eventQueue s)) }
    return e

queueEmpty :: Monad m => VirtualMachine m Bool
queueEmpty = do
    q <- eventQueue `liftM` get
    return $ S.null q

getVmHandle :: Monad m => VirtualMachine m Handle
getVmHandle = vmHandle `liftM` get

setVmHandle :: Monad m => Handle -> VirtualMachine m ()
setVmHandle h = do
    s <- get
    put $ s { vmHandle = h}

getIdSizes :: Monad m => VirtualMachine m J.IdSizes
getIdSizes = do
    is <- idSizesConf `liftM` get
    case is of
        Nothing -> fail "idSizes have not been set yet"
        Just v  -> return v

setIdSizes :: Monad m => J.IdSizes -> VirtualMachine m ()
setIdSizes iss = do
    s <- get
    put $ s { idSizesConf = (Just iss) }

getJdwpVersion :: Monad m => VirtualMachine m JdwpVersion
getJdwpVersion = do
    v <- jdwpVersion `liftM` get
    case v of
        Nothing -> fail "version hasn't been set yet"
        Just v  -> return v

setJdwpVersion :: Monad m => JdwpVersion -> VirtualMachine m ()
setJdwpVersion v = do
    s <- get
    put $ s { jdwpVersion = (Just v) }

getCapabilities :: Monad m => VirtualMachine m J.Capabilities
getCapabilities = do
    c <- capabilities `liftM` get
    case c of
        Nothing -> fail "capabilities are not set"
        Just v  -> return v

setCapabilities :: Monad m => J.Capabilities -> VirtualMachine m ()
setCapabilities c = do
    s <- get
    put $ s { capabilities = (Just c) }

handshake :: (Error e, MonadIO m, MonadError e m) => Handle -> VirtualMachine m ()
handshake h = do
    liftIO $ hPutStr h "JDWP-Handshake"
    liftIO $ hFlush h
    value <- liftIO $ B.hGet h 14
    when (value /= (B8.pack "JDWP-Handshake")) $ throwError $ strMsg "Handshake failed"
-- }}}

-- runVirtualMachine section {{{
{- | Executes source code which communicates with virtual machine.
 Source code is executed for Vm running on the defined host and port.
 -}
runVirtualMachine :: (Error e, MonadIO m, MonadError e m) =>
       String -- ^ Host address.
    -> PortID -- ^ Port.
    -> VirtualMachine m () -- ^ Monad to run.
    -> m () -- ^ Internal monad.
runVirtualMachine host port vm = do
    h <- liftIO $ connectTo host port
    liftIO $ hSetBinaryMode h True
    runStateT ((handshake h) >> (preflight >> vm >> releaseResources))
              (initialVmState h)
    return ()

preflight :: MonadIO m => VirtualMachine m ()
preflight = do
    h <- getVmHandle
    firstPacketData <- J.dat `liftM` (liftIO $ J.waitEvent h)
    askIdSizes
    idsizes <- getIdSizes
    let vmStartEventSet = runGet (J.parseEventSet idsizes) (J.toLazy firstPacketData)
    addToQueue vmStartEventSet
    askJdwpVersion

askIdSizes :: MonadIO m => VirtualMachine m ()
askIdSizes = do
    h <- getVmHandle
    liftIO $ putStrLn $ show h
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.idSizesCommand cntr
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    setIdSizes $ runGet J.parseIdSizes (J.toLazy r)

askJdwpVersion :: MonadIO m => VirtualMachine m ()
askJdwpVersion = do
    v <- runVersionCommand
    setJdwpVersion $ JdwpVersion (J.jdwpMajor v) (J.jdwpMinor v)

askCapabilities :: MonadIO m => VirtualMachine m ()
askCapabilities = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    idsizes <- getIdSizes
    jdwpVersion <- getJdwpVersion
    if jdwpVersion >= (JdwpVersion 1 4)
        then liftIO $ J.sendPacket h $ J.capabilitiesNewCommand cntr
        else liftIO $ J.sendPacket h $ J.capabilitiesCommand cntr

releaseResources :: MonadIO m => VirtualMachine m ()
releaseResources = do
    s <- get
    liftIO $ hClose $ vmHandle s

initialVmState :: Handle -> VmState
initialVmState h = VmState Nothing 0 h S.empty Nothing Nothing
-- }}}

-- Classes definitions. {{{

class Name a where
    name :: (Error e, MonadIO m, MonadError e m) => a -> VirtualMachine m String

class Resumable a where
    resume :: (Error e, MonadIO m, MonadError e m) => a -> VirtualMachine m ()

class Locatable a where
    location :: (Error e, MonadIO m, MonadError e m) => a -> VirtualMachine m Location

class SourceName a where
    sourceName :: (Error e, MonadIO m, MonadError e m) => a -> VirtualMachine m String

class AllLineLocations a where
    allLineLocations :: (Error e, MonadIO m, MonadError e m) => a -> VirtualMachine m [Location]

-- }}}

--- Functions from official interface {{{

-- VirtualMachine functions section {{{

-- | Returns the name of the target VM as reported by the property java.vm.name.
vmName :: MonadIO m => VirtualMachine m String
vmName = J.vmName `liftM` runVersionCommand

-- | Returns text information on the target VM
-- and the debugger support that mirrors it.
description :: MonadIO m => VirtualMachine m String
description = J.description `liftM` runVersionCommand

-- | The version of the virtual machine.
version :: MonadIO m => VirtualMachine m String
version = J.vmVersion `liftM` runVersionCommand

{- | Returns all loaded types. For each loaded type in the target VM a
 ReferenceType will be placed in the returned list. The list will include
 ReferenceTypes which mirror classes, interfaces, and array types.

 The returned list will include reference types loaded at least to the point
 of preparation and types (like array) for which preparation is not defined.
 -}
allClasses :: MonadIO m => VirtualMachine m [J.ReferenceType]
allClasses = do
    h <- getVmHandle
    idsizes <- getIdSizes
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.allClassesCommand cntr
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let classes = runGet (J.parseAllClassesReply idsizes) (J.toLazy r)
    return classes

-- | Returns a list of the currently running threads.
allThreads :: MonadIO m => VirtualMachine m [J.ThreadReference]
allThreads = do
    h <- getVmHandle
    idsizes <- getIdSizes
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.allThreadsCommand cntr
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let threads = runGet (J.parseAllThreadsReply idsizes) (J.toLazy r)
    return threads

{- | Determines if the target VM supports the addition
     of methods when performing class redefinition. -}
canAddMethod :: MonadIO m => VirtualMachine m Bool
canAddMethod = J.canAddMethod `liftM` getCapabilities

-- | Determines if the target VM is a read-only VM.
canBeModified :: MonadIO m => VirtualMachine m Bool
canBeModified = undefined -- I'm not sure what should be here right now.
                          -- Different JDI implementations put just constant
                          -- values true or false. TODO

-- | Determines if the target VM supports the retrieval of a method's bytecodes.
canGetBytecodes :: MonadIO m => VirtualMachine m Bool
canGetBytecodes = J.canGetBytecodes `liftM` getCapabilities

{- | Determines if the target VM supports the retrieval of the monitor
     for which a thread is currently waiting.-}
canGetCurrentContendedMonitor :: MonadIO m => VirtualMachine m Bool
canGetCurrentContendedMonitor =
                J.canGetCurrentContendedMonitor `liftM` getCapabilities

{- | Determines if the target VM supports the retrieval
     of the monitor information for an object.-}
canGetMonitorInfo :: MonadIO m => VirtualMachine m Bool
canGetMonitorInfo = J.canGetMonitorInfo `liftM` getCapabilities

{- | Determines if the target VM supports the retrieval
     of the monitors owned by a thread.-}
canGetOwnedMonitorInfo :: MonadIO m => VirtualMachine m Bool
canGetOwnedMonitorInfo = J.canGetOwnedMonitorInfo `liftM` getCapabilities

-- | Determines if the target VM supports getting the source debug extension.
canGetSourceDebugExtension :: MonadIO m => VirtualMachine m Bool
canGetSourceDebugExtension =
                        J.canGetSourceDebugExtension `liftM` getCapabilities

{- | Determines if the target VM supports the query of the synthetic attribute
     of a method or field.-}
canGetSynteticAttribute :: MonadIO m => VirtualMachine m Bool
canGetSynteticAttribute = J.canGetSynteticAttribute `liftM` getCapabilities

-- | Determines if the target VM supports popping frames of a threads stack.
canPopFrames :: MonadIO m => VirtualMachine m Bool
canPopFrames = J.canPopFrames `liftM` getCapabilities

-- | Determines if the target VM supports any level of class redefinition.
canRedefineClasses :: MonadIO m => VirtualMachine m Bool
canRedefineClasses = J.canRedefineClasses `liftM` getCapabilities

-- | Determines if the target VM supports the creation of VMDeathRequests.
canRequestVmDeathEvent :: MonadIO m => VirtualMachine m Bool
canRequestVmDeathEvent = J.canRequestVmDeathEvent `liftM` getCapabilities

{- | Determines if the target VM supports unrestricted changes
     when performing class redefinition.-}
canUnrestrictedlyRedefineClasses :: MonadIO m => VirtualMachine m Bool
canUnrestrictedlyRedefineClasses =
                J.canUnrestrictedlyRedefineClasses `liftM` getCapabilities


{- | Returns the loaded reference types that match a given name. The name must
 be fully qualified (for example, java.lang.String). The returned list will
 contain a ReferenceType for each class or interface found with the given name.
 The search is confined to loaded classes only; no attempt is made to load a
 class of the given name.

 The returned list will include reference types loaded at least to the point
 of preparation and types (like array) for which preparation is not defined.
-}
classesByName :: (Error e, MonadIO m, MonadError e m) =>
       String -- ^ className - the class/interface name to search for
    -> VirtualMachine m [J.ReferenceType] {- ^ a list of ReferenceType objects,
                                          each mirroring a type in the target
                                          VM with the given name. -}
classesByName name = do
    let jniName = "L" ++ (map replaceDot name) ++ ";"
    idsizes <- getIdSizes
    reply <- runCommand $ J.classesBySignatureCommand jniName
    let r = J.dat reply
    let classes = runGet (J.parseClassesBySignatureReply idsizes) (J.toLazy r)
    return $ map (setSignature jniName) classes

    where
        replaceDot '.' = '/'
        replaceDot x = x
        setSignature newSig (J.ReferenceType tt ri _ cs) =
            J.ReferenceType tt ri newSig cs

{- | Determines if the target VM supports filtering events
     by specific instance object.-}
canUseInstanceFilters :: MonadIO m => VirtualMachine m Bool
canUseInstanceFilters = J.canUseInstanceFilters `liftM` getCapabilities

-- | Determines if the target VM supports watchpoints for field access.
canWatchFieldAccess :: MonadIO m => VirtualMachine m Bool
canWatchFieldAccess = J.canWatchFieldAccess `liftM` getCapabilities

-- | Determines if the target VM supports watchpoints for field modification.
canWatchFieldModification :: MonadIO m => VirtualMachine m Bool
canWatchFieldModification = J.canWatchFieldModification `liftM` getCapabilities

-- | Invalidates this virtual machine mirror.
dispose :: MonadIO m => VirtualMachine m ()
dispose = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.disposeCommand cntr
    liftIO $ J.waitReply h
    return ()

-- | Causes the mirrored VM to terminate with the given error code.
exit :: MonadIO m => Int -> VirtualMachine m ()
exit exitCode = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.exitCommand cntr (fromIntegral exitCode)
    liftIO $ J.waitReply h
    return ()

-- | Continues the execution of the application running in this virtual machine.
resumeVm :: MonadIO m => VirtualMachine m ()
resumeVm = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    idsizes <- getIdSizes
    liftIO $ J.sendPacket h $ J.resumeVmCommand cntr
    r <- liftIO $ J.waitReply h
    return ()

-- | Returns each thread group which does not have a parent.
topLevelThreadGroups :: MonadIO m => VirtualMachine m [J.ThreadGroupReference]
topLevelThreadGroups = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    idsizes <- getIdSizes
    liftIO $ J.sendPacket h $ J.topLevelThreadGroupsCommand cntr
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let groups = runGet (J.parseThreadGroupsReply idsizes) (J.toLazy r)
    return groups

-- }}}

-- Event functions section {{{
instance Locatable J.Event where
    location (J.BreakpointEvent _ _ javaLocation) =
        locationFromJavaLocation javaLocation
    location (J.StepEvent _ _ javaLocation) =
        locationFromJavaLocation javaLocation

-- | Returns the reference type for which this event was generated.
referenceType :: J.Event -> J.ReferenceType
referenceType (J.ClassPrepareEvent
                    _
                    threadId
                    typeTag
                    typeId
                    signature
                    classStatus) = J.ReferenceType
                                            typeTag
                                            typeId
                                            signature
                                            classStatus

{- | Returns the thread in which this event has occurred.

 In rare cases, this event may occur in a debugger system thread within the
 target VM. Debugger threads take precautions to prevent these events, but
 they cannot be avoided under some conditions, especially for some subclasses
 of Error. If the event was generated by a debugger system thread, the value
 returned by this method is null, and if the requested suspend policy for the
 event was EventRequest.SUSPEND_EVENT_THREAD, all threads will be suspended
 instead, and the EventSet.suspendPolicy() will reflect this change.

 Note that the discussion above does not apply to system threads created by the
 target VM during its normal (non-debug) operation.
-}
thread :: J.Event -> J.ThreadReference
thread (J.ClassPrepareEvent
            _
            threadId
            _ _ _ _) = J.ThreadReference threadId
thread (J.BreakpointEvent
            _
            threadId
            _) = J.ThreadReference threadId
thread (J.StepEvent
            _
            threadId
            _) = J.ThreadReference threadId

-- }}}

-- EventSet functions section {{{
instance Resumable J.EventSet where
    resume (J.EventSet J.SuspendAll _) = resumeVm
    resume (J.EventSet J.SuspendEventThread events)
                                       = resumeThreadId
                                       $ J.threadId
                                       $ head events
    resume (J.EventSet J.SuspendNone _) = return ()

removeEvent :: MonadIO m => VirtualMachine m J.EventSet
removeEvent = do
    h <- getVmHandle
    idsizes <- getIdSizes
    qe <- queueEmpty
    if qe
        then do
            eventSetData <- J.dat `liftM` (liftIO $ J.waitEvent h)
            return $ runGet (J.parseEventSet idsizes) (J.toLazy eventSetData)
        else takeFromQueue
-- }}}

-- EventRequest functions section. {{{

data EventRequest = EventRequest
                        J.SuspendPolicy
                        (Maybe J.JavaInt) -- Id of event request if it's enabled
                        [J.EventModifier]
                        EventRequest
                  | ClassPrepareRequest
                  | BreakpointRequest Location
                  | StepRequest J.ThreadReference J.StepSize J.StepDepth
                    deriving (Show, Eq)

enable :: (Error e, MonadIO m, MonadError e m) => EventRequest -> VirtualMachine m EventRequest
enable (EventRequest suspendPolicy Nothing modifiers ClassPrepareRequest) = do
    reply <- runCommand $ J.eventSetRequest J.ClassPrepare suspendPolicy modifiers
    let r = J.dat reply
    let requestId = runGet J.parseInt (J.toLazy r)
    return $ EventRequest suspendPolicy (Just requestId) modifiers ClassPrepareRequest
enable (EventRequest suspendPolicy Nothing modifiers request@(BreakpointRequest
                (Location (J.ReferenceType typeTag refId _ _)
                          (J.Method mId _ _ _)
                          (J.Line codeIndex lineNum)))) = do
    let modifiers' = (J.LocationOnly
                         (J.JavaLocation typeTag refId mId codeIndex)
                     ) : modifiers
    reply <- runCommand $ J.eventSetRequest J.Breakpoint suspendPolicy modifiers'
    let r = J.dat reply
    let requestId = runGet J.parseInt (J.toLazy r)
    return $ EventRequest suspendPolicy (Just requestId) modifiers request
enable (EventRequest suspendPolicy Nothing modifiers request@(StepRequest
                (J.ThreadReference tId) ss sd)) = do
    let modifiers' = (J.Step tId ss sd) : modifiers
    reply <- runCommand $ J.eventSetRequest J.SingleStep suspendPolicy modifiers'
    let r = J.dat reply
    let requestId = runGet J.parseInt (J.toLazy r)
    return $ EventRequest suspendPolicy (Just requestId) modifiers request
enable request@(EventRequest suspendPolicy (Just _) _ _) = return request

isEnabled :: EventRequest -> Bool
isEnabled (EventRequest _ (Just _) _ _) = True
isEnabled (EventRequest _ Nothing _ _)  = False

disable :: (Error e, MonadIO m, MonadError e m) => EventRequest -> VirtualMachine m EventRequest
disable (EventRequest
                suspendPolicy
                (Just requestId)
                modifiers
                er@(ClassPrepareRequest{})) =
    runCommand (J.eventClearRequest J.ClassPrepare requestId) >>
    return (EventRequest suspendPolicy Nothing modifiers er)

addCountFilter :: Int -> EventRequest -> EventRequest
addCountFilter count (EventRequest sp ri ems er) =
            EventRequest sp ri ((J.Count (fromIntegral count)) : ems) er

createClassPrepareRequest :: EventRequest
createClassPrepareRequest = EventRequest J.SuspendAll Nothing [] ClassPrepareRequest

createBreakpointRequest :: Location -> EventRequest
createBreakpointRequest l = EventRequest J.SuspendAll Nothing [] (BreakpointRequest l)

createStepRequest :: J.ThreadReference -> J.StepSize -> J.StepDepth -> EventRequest
createStepRequest tr ss sd = EventRequest J.SuspendAll Nothing [] (StepRequest tr ss sd)

-- }}}

-- ReferenceType functions section {{{

genericSignature :: J.ReferenceType -> String
genericSignature (J.ReferenceType _ _ gs _) = gs

allFields :: (Error e, MonadIO m, MonadError e m) =>
             J.ReferenceType -> VirtualMachine m [Field]
allFields rt@(J.ReferenceType _ refId _ _) = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.fieldsCommand refId
    let r = J.dat reply
    let fields = runGet (J.parseFieldsReply idsizes) (J.toLazy r)
    return $ map (Field rt) fields

allMethods :: (Error e, MonadIO m, MonadError e m) =>
              J.ReferenceType -> VirtualMachine m [Method]
allMethods rt@(J.ReferenceType _ refId _ _) = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.methodsCommand refId
    let r = J.dat reply
    let methods = runGet (J.parseMethodsReply idsizes) (J.toLazy r)
    return $ map (Method rt) methods

instance Name J.ReferenceType where
    name = return . signatureToName . genericSignature

instance SourceName J.ReferenceType where
    sourceName (J.ReferenceType _ refId _ _) = do
        reply <- runCommand $ J.sourceFileCommand refId
        let r = J.dat reply
        let sourceName = runGet J.parseString (J.toLazy r)
        return sourceName

instance AllLineLocations J.ReferenceType where
    allLineLocations refType = concat `liftM` ((mapM allLineLocations) =<< (allMethods refType))

-- }}}

-- ArrayReference functions section {{{

getArrValue :: (Error e, MonadIO m, MonadError e m) =>
               J.ArrayReference -> Int -> VirtualMachine m Value
getArrValue arrRef@(J.ArrayReference objId) index = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.getArrayValuesCommand objId (fromIntegral index) 1
    let r = J.dat reply
    let values = runGet (J.parseArrayRegion idsizes) (J.toLazy r)
    toJdiValue $ head values

getArrValues :: (Error e, MonadIO m, MonadError e m) =>
                J.ArrayReference -> VirtualMachine m [Value]
getArrValues arrRef@(J.ArrayReference objId) = do
    idsizes <- getIdSizes
    l <- arrLength arrRef
    reply <- runCommand $ J.getArrayValuesCommand objId 0 (fromIntegral l)
    let r = J.dat reply
    let values = runGet (J.parseArrayRegion idsizes) (J.toLazy r)
    mapM toJdiValue values

arrLength :: (Error e, MonadIO m, MonadError e m) =>
             J.ArrayReference -> VirtualMachine m Int
arrLength arrRef@(J.ArrayReference objId) = do
    reply <- runCommand $ J.lengthCommand objId
    let r = J.dat reply
    let value = runGet J.parseInt (J.toLazy r)
    return $ fromIntegral value

-- }}}

-- StringReference functions section {{{

stringValue :: (Error e, MonadIO m, MonadError e m) =>
                 J.StringReference -> VirtualMachine m String
stringValue sr@(J.StringReference sid) = do
    reply <- runCommand $ J.stringValueCommand sid
    return $ runGet J.parseString (J.toLazy $ J.dat reply)

-- }}}

-- Value functions section {{{

data Value = ArrayValue J.ArrayReference
           | ByteValue Int
           | CharValue Char
       --    | ObjectValue ObjectReference
           | FloatValue Float
           | DoubleValue Double
           | IntValue Int
           | LongValue Int
           | ShortValue Int
           | VoidValue
           | BooleanValue Bool
           | StringValue J.StringReference
           | ThreadValue J.ThreadReference
           | ThreadGroupValue J.ThreadGroupReference
       --    | ClassLoaderValue
       --    | ClassObjectValue
             deriving (Eq, Show)

toJdiValue :: (Monad m, Error e, MonadError e m) =>
              (J.Value -> m Value)
toJdiValue (J.BooleanValue v) = return (BooleanValue $ v /= 0)

toJdiValue (J.ByteValue v) = return (ByteValue $ fromIntegral v)

toJdiValue (J.CharValue v) = return (CharValue $ toEnum $ fromIntegral v)

toJdiValue (J.IntValue v) = return (IntValue $ fromIntegral v)

toJdiValue (J.LongValue v) = return (LongValue $ fromIntegral v)

toJdiValue (J.ArrayValue objectId) = return (ArrayValue $
                                                J.ArrayReference objectId)

toJdiValue (J.StringValue objectId) = return (StringValue $
                                                J.StringReference objectId)
toJdiValue _ = throwError $ strMsg "unknown value yet"

-- }}}

-- StackFrame functions section {{{

data StackFrame = StackFrame J.ThreadReference J.StackFrame
                  deriving (Eq, Show)

getValue :: (Error e, MonadIO m, MonadError e m) =>
            StackFrame -> LocalVariable -> VirtualMachine m Value
getValue (StackFrame (J.ThreadReference ti) (J.StackFrame fi _))
         (LocalVariable _ _ slot) = do
    reply <- runCommand $ J.getValuesCommand ti fi [slot]
    idsizes <- getIdSizes
    toJdiValue $ head $ runGet
                        (J.parseGetValuesReply idsizes)
                        (J.toLazy $ J.dat reply)

instance Locatable StackFrame where
    location (StackFrame _ (J.StackFrame _ javaLoc))
                        = locationFromJavaLocation javaLoc

-- }}}

-- ThreadReference functions section {{{

instance Name J.ThreadReference where
    name (J.ThreadReference refId) = do
        reply <- runCommand $ J.threadReferenceNameCommand refId
        let r = J.dat reply
        let name = runGet J.parseString (J.toLazy r)
        return name

instance Resumable J.ThreadReference where
    resume (J.ThreadReference tId) = resumeThreadId tId

allFrames :: (Error e, MonadIO m, MonadError e m) => J.ThreadReference -> VirtualMachine m [StackFrame]
allFrames tr = getFrames tr 0 (-1)

frameCount :: (Error e, MonadIO m, MonadError e m) => J.ThreadReference -> VirtualMachine m Int
frameCount tr@(J.ThreadReference tId) = do
    reply <- runCommand $ J.frameCountCommand tId
    let count = runGet J.parseInt (J.toLazy $ J.dat reply)
    return $ fromIntegral count

frames :: (Error e, MonadIO m, MonadError e m) =>
          J.ThreadReference -> Int -> Int -> VirtualMachine m [StackFrame]
frames tr start len = do
    when (start < 0) $ throwError $ strMsg "negative start"
    when (len < 0) $ throwError $ strMsg "negative len"
    getFrames tr start len

-- This is unsafe implementation of frames command.
getFrames :: (Error e, MonadIO m, MonadError e m) => J.ThreadReference -> Int -> Int -> VirtualMachine m [StackFrame]
getFrames tr@(J.ThreadReference ti) start len = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.framesCommand ti (fromIntegral start) (fromIntegral len)
    let r = J.dat reply
    return $ map (StackFrame tr) $ runGet (J.parseStackFrameList idsizes) (J.toLazy r)

-- }}}

-- Field functions section {{{

data Field = Field J.ReferenceType J.Field
              deriving (Eq, Show)

-- }}}

-- Method functions section {{{

data Method = Method J.ReferenceType J.Method
              deriving (Eq, Show)

instance Name Method where
    name (Method _ (J.Method _ name _ _)) = return name

instance AllLineLocations Method where
    allLineLocations m@(Method ref method) = do
        (J.LineTable _ _ lines) <- receiveLineTable m
        return $ map (Location ref method) lines

instance Locatable Method where
    location m@(Method ref method) = do
        (J.LineTable _ _ lines) <- receiveLineTable m
        return $ Location ref method (head lines)

arguments :: (Error e, MonadIO m, MonadError e m) =>
             Method -> VirtualMachine m [LocalVariable]
arguments method = getVariables method (>)

variables :: (Error e, MonadIO m, MonadError e m) =>
             Method -> VirtualMachine m [LocalVariable]
variables method = getVariables method (<=)

variablesByName :: (Error e, MonadIO m, MonadError e m) =>
                   Method -> String -> VirtualMachine m [LocalVariable]
variablesByName method varName =
    variables method >>= filterM (((varName ==) `liftM`) . name)
-- }}}

-- LocalVariable functions section {{{

data LocalVariable = LocalVariable J.ReferenceType J.Method J.Slot
                     deriving (Show, Eq)

instance Name LocalVariable where
    name (LocalVariable _ _ (J.Slot _ nm _ _ _)) = return nm

-- }}}

-- Location functions section {{{

data Location = Location J.ReferenceType J.Method J.Line
                deriving (Show, Eq)

codeIndex :: Location -> Int
codeIndex (Location _ _ (J.Line ci _)) = fromIntegral ci

declaringType :: Location -> J.ReferenceType
declaringType (Location rt _ _) = rt

lineNumber :: Location -> Int
lineNumber (Location _ _ (J.Line _ ln)) = fromIntegral ln

method :: Location -> Method
method (Location refType method _) = Method refType method

instance SourceName Location where
    sourceName (Location ref _ _) = sourceName ref

-- }}}

-- }}}

--- Auxiliary functions {{{
runVersionCommand :: MonadIO m => VirtualMachine m J.Version
runVersionCommand = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.versionCommand cntr
    p <- liftIO $ J.waitReply h
    return $ runGet J.parseVersion (J.toLazy $ J.dat p)

receiveLineTable :: (Error e, MonadIO m, MonadError e m) => Method -> VirtualMachine m J.LineTable
receiveLineTable (Method (J.ReferenceType _ refId _ _)
                         (J.Method mId _ _ _)) = do
    reply <- runCommand $ J.lineTableCommand refId mId
    let r = J.dat reply
    return $ runGet J.parseLineTableReply (J.toLazy r)

resumeThreadId :: (Error e, MonadIO m, MonadError e m) => J.JavaThreadId -> VirtualMachine m ()
resumeThreadId tId = runCommand (J.resumeThreadCommand tId) >> return ()

referenceTypeFromRefId :: (Error e, MonadIO m, MonadError e m) =>
                          J.TypeTag -> J.JavaClassId -> VirtualMachine m J.ReferenceType
referenceTypeFromRefId typeTag refId = do
    reply <- runCommand $ J.signatureCommand refId
    let csr = J.dat reply
    let classSignature = runGet J.parseString (J.toLazy csr)
    reply' <- runCommand $ J.statusCommand refId
    let str = J.dat reply'
    let status = runGet J.parseClassStatus (J.toLazy str)
    return $ J.ReferenceType typeTag refId classSignature status

locationFromJavaLocation :: (Error e, MonadIO m, MonadError e m) =>
                            J.JavaLocation -> VirtualMachine m Location
locationFromJavaLocation (J.JavaLocation typeTag refId methodId index) = do
    rt <- referenceTypeFromRefId typeTag refId
    methodList <- allMethods rt
    let (Just method) = find isMyMethod methodList
    al <- allLineLocations method
    return $ last $ filter (lessThanIndex index) al
    where
        isMyMethod (Method _ (J.Method id _ _ _)) = id == methodId
        lessThanIndex index (Location _ _ (J.Line codeIndex _))  = codeIndex <= index

signatureToName :: String -> String
signatureToName "Z" = "boolean"
signatureToName "B" = "byte"
signatureToName "C" = "char"
signatureToName "S" = "short"
signatureToName "I" = "int"
signatureToName "J" = "long"
signatureToName "F" = "float"
signatureToName "D" = "double"
signatureToName ('L' : v) = (flip map) (init v) $
    \x -> case x of
        '/' -> '.'
        v   -> v
signatureToName ('[' : v) = (signatureToName v) ++ "[]"

type SlotComparator = (J.JavaInt -> J.JavaInt -> Bool)

runCommand :: (Error e, MonadIO m, MonadError e m) =>
              (J.PacketId -> J.Packet) -> VirtualMachine m J.Packet
runCommand packet = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ packet cntr
    r <- liftIO $ J.waitReply h
    let errCode = fromIntegral $ J.errorCode r
    when (errCode /= 0) $ throwError $ strMsg $ J.errorFromCode errCode
    return r

getVariables :: (Error e, MonadIO m, MonadError e m) =>
                Method -> SlotComparator -> VirtualMachine m [LocalVariable]
getVariables (Method
            ref@(J.ReferenceType _ refId _ _)
            m@(J.Method mId _ _ _)) comparator = do
    reply <- runCommand $ J.variableTableCommand refId mId
    if (J.errorCode reply) /= 0
        then throwError $ strMsg "Information about variables is absent"
        else do
            let (J.VariableTable argCount varList) = runGet J.parseVariableTableReply (J.toLazy $ J.dat reply)
            let slots = filter ((argCount `comparator`) . slot) varList
            return $ map (LocalVariable ref m) slots
    where
        slot (J.Slot _ _ _ _ s) = s

-- }}}

-- vim: foldmethod=marker foldmarker={{{,}}}

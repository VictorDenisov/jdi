module Language.Java.Jdi.Impl where

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
import Data.Bits ((.&.))
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
allThreads :: (Error e, MonadIO m, MonadError e m)
           => VirtualMachine m [ThreadReference]
allThreads = do
    h <- getVmHandle
    idsizes <- getIdSizes
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.allThreadsCommand cntr
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let threads = runGet (J.parseAllThreadsReply idsizes) (J.toLazy r)
    mapM threadReferenceFromId threads

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

resumeVm :: (Error e, MonadIO m, MonadError e m) => VirtualMachine m ()
resumeVm = runCommand J.resumeVmCommand >> return ()

suspendVm :: (Error e, MonadIO m, MonadError e m) => VirtualMachine m ()
suspendVm = runCommand J.suspendVmCommand >> return ()

-- | Returns each thread group which does not have a parent.
topLevelThreadGroups :: (Error e, MonadIO m, MonadError e m)
                     => VirtualMachine m [ThreadGroupReference]
topLevelThreadGroups = do
    reply <- runCommand J.topLevelThreadGroupsCommand
    let r = J.dat reply
    idsizes <- getIdSizes
    let groups = runGet (J.parseThreadGroupsReply idsizes) (J.toLazy r)
    mapM threadGroupReferenceFromId groups

-- }}}

-- Event functions section {{{

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
thread :: (Error e, MonadIO m, MonadError e m)
       => J.Event -> VirtualMachine m ThreadReference
thread (J.ClassPrepareEvent
            _
            threadId
            _ _ _ _) = threadReferenceFromId threadId
thread (J.BreakpointEvent
            _
            threadId
            _) = threadReferenceFromId threadId
thread (J.StepEvent
            _
            threadId
            _) = threadReferenceFromId threadId

-- }}}

-- EventSet functions section {{{
resumeEventSet :: (Error e, MonadIO m, MonadError e m)
               => J.EventSet -> VirtualMachine m ()
resumeEventSet (J.EventSet J.SuspendAll _) = resumeVm
resumeEventSet (J.EventSet J.SuspendEventThread events)
                                   = resumeThreadId
                                   $ J.threadId
                                   $ head events
resumeEventSet (J.EventSet J.SuspendNone _) = return ()

{- | Waits forever for the next available event.

Returns: the next EventSet.
-}
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

{- | Represents a request for notification of an event. Examples include
BreakpointRequest and ExceptionRequest. When an event occurs for which an
enabled request is present, an EventSet will be placed on the event queue.

The number of events generated for an event request can be controlled through
filters. Filters provide additional constraints that an event must satisfy
before it is placed on the event queue. Multiple filters can be used by making
multiple calls to filter addition functions such as
addClassFilter :: EventRequest -> String -> EventRequest. Filters are
added to an event one at a time only while the event is disabled.
Multiple filters are applied with CUT-OFF AND, in the order it was added to
the request. Only events that satisfy all filters are placed in the event queue.

The set of available filters is dependent on the event request, some examples
of filters are:

    Thread filters allow control over the thread for which events are generated.
    Class filters allow control over the class in which the event occurs.
    Instance filters allow control over the instance in which the event occurs.
    Count filters allow control over the number of times an event is reported.

Filters can dramatically improve debugger performance by reducing the amount of
event traffic sent from the target VM to the debugger VM.
-}
data EventRequest = EventRequest
                        J.SuspendPolicy
                        (Maybe J.JavaInt) -- Id of event request if it's enabled
                        [J.EventModifier]
                        EventRequest
                  | ClassPrepareRequest
                  | BreakpointRequest Location
                  | StepRequest ThreadReference J.StepSize J.StepDepth
                    deriving (Show, Eq)
{- | Enables the event request and returns descriptor of the enabled event
request. The descriptor can be used to disable the event request.

While this event request is disabled, the event request will be ignored and the
target VM will not be stopped if any of its threads reaches the event request.
Disabled event requests still exist, and are included in event request lists.
-}
enable :: (Error e, MonadIO m, MonadError e m)
       => EventRequest -> VirtualMachine m EventRequest
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
                (ThreadReference _ tId) ss sd)) = do
    let modifiers' = (J.Step tId ss sd) : modifiers
    reply <- runCommand $ J.eventSetRequest J.SingleStep suspendPolicy modifiers'
    let r = J.dat reply
    let requestId = runGet J.parseInt (J.toLazy r)
    return $ EventRequest suspendPolicy (Just requestId) modifiers request
enable request@(EventRequest suspendPolicy (Just _) _ _) = return request

-- | Determines if this event request is currently enabled.
isEnabled :: EventRequest -> Bool
isEnabled (EventRequest _ (Just _) _ _) = True
isEnabled (EventRequest _ Nothing _ _)  = False

{- | Disables the event request and returns descriptor of the disabled event
request. The descriptor can be used to enable the event request again.

While this event request is disabled, the event request will be ignored and the
target VM will not be stopped if any of its threads reaches the event request.
Disabled event requests still exist, and are included in event request lists.
-}
disable :: (Error e, MonadIO m, MonadError e m)
        => EventRequest -> VirtualMachine m EventRequest
disable (EventRequest
                suspendPolicy
                (Just requestId)
                modifiers
                er@(ClassPrepareRequest{})) =
    runCommand (J.eventClearRequest J.ClassPrepare requestId) >>
    return (EventRequest suspendPolicy Nothing modifiers er)

{- | Limit the requested event to be reported at most once after a given number
of occurrences. The event is not reported the first count - 1 times this filter
is reached. To request a one-off event, call this method with a count of 1.

Once the count reaches 0, any subsequent filters in this request are applied.
If none of those filters cause the event to be suppressed, the event is
reported. Otherwise, the event is not reported. In either case subsequent events
are never reported for this request.
-}
addCountFilter :: Int -> EventRequest -> EventRequest
addCountFilter count (EventRequest sp ri ems er) =
            EventRequest sp ri ((J.Count (fromIntegral count)) : ems) er

-- | Creates a new disabled ClassPrepareRequest.
createClassPrepareRequest :: EventRequest
createClassPrepareRequest = EventRequest J.SuspendAll Nothing [] ClassPrepareRequest

-- | Creates a new disabled BreakpointRequest.
createBreakpointRequest :: Location -> EventRequest
createBreakpointRequest l = EventRequest J.SuspendAll Nothing [] (BreakpointRequest l)

-- | Creates a new disabled StepRequest.
createStepRequest :: ThreadReference -> J.StepSize -> J.StepDepth -> EventRequest
createStepRequest tr ss sd = EventRequest J.SuspendAll Nothing [] (StepRequest tr ss sd)

-- }}}

-- ReferenceType functions section {{{

{- | Returns a list containing each 'Method' declared directly in this type.
Inherited methods are not included. Constructors, the initialization method if
any, and any synthetic methods created by the compiler are included in the list.

For arrays ('ArrayType') and primitive classes, the returned list is always
empty.
-}
methods :: (Error e, MonadIO m, MonadError e m) =>
           J.ReferenceType -> VirtualMachine m [Method]
methods rt@(J.ReferenceType _ refId _ _) = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.methodsCommand refId
    let r = J.dat reply
    let methods = runGet (J.parseMethodsReply idsizes) (J.toLazy r)
    return $ map (Method rt) methods

-- }}}

-- ArrayReference functions section {{{

getArrValue :: (Error e, MonadIO m, MonadError e m) =>
               J.ArrayReference -> Int -> VirtualMachine m Value
getArrValue arrRef@(J.ArrayReference objId) index = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.getArrayValuesCommand objId (fromIntegral index) 1
    let r = J.dat reply
    let values = runGet (J.parseArrayRegion idsizes) (J.toLazy r)
    return $ toJdiValue $ head values

getArrValues :: (Error e, MonadIO m, MonadError e m) =>
                J.ArrayReference -> VirtualMachine m [Value]
getArrValues arrRef@(J.ArrayReference objId) = do
    idsizes <- getIdSizes
    l <- arrLength arrRef
    reply <- runCommand $ J.getArrayValuesCommand objId 0 (fromIntegral l)
    let r = J.dat reply
    let values = runGet (J.parseArrayRegion idsizes) (J.toLazy r)
    return $ map toJdiValue values

arrLength :: (Error e, MonadIO m, MonadError e m) =>
             J.ArrayReference -> VirtualMachine m Int
arrLength arrRef@(J.ArrayReference objId) = do
    reply <- runCommand $ J.lengthCommand objId
    let r = J.dat reply
    let value = runGet J.parseInt (J.toLazy r)
    return $ fromIntegral value

-- }}}

-- StringReference functions section {{{

-- }}}

-- Value functions section {{{

data Value = ArrayValue J.ArrayReference
           | ByteValue Int
           | CharValue Char
           | ObjectValue J.ObjectReference
           | FloatValue Float
           | DoubleValue Double
           | IntValue Int
           | LongValue Int
           | ShortValue Int
           | VoidValue
           | BooleanValue Bool
           | StringValue J.StringReference
           | ThreadValue ThreadReference
           | ThreadGroupValue ThreadGroupReference
       --    | ClassLoaderValue
       --    | ClassObjectValue
             deriving (Eq, Show)

toJdiValue :: J.Value -> Value
toJdiValue (J.BooleanValue v) = BooleanValue $ v /= 0

toJdiValue (J.ByteValue v) = ByteValue $ fromIntegral v

toJdiValue (J.CharValue v) = CharValue $ toEnum $ fromIntegral v

toJdiValue (J.IntValue v) = IntValue $ fromIntegral v

toJdiValue (J.LongValue v) = LongValue $ fromIntegral v

toJdiValue (J.ArrayValue objectId) = ArrayValue $ J.ArrayReference objectId

toJdiValue (J.StringValue objectId) = StringValue $ J.StringReference objectId

toJdiValue (J.ObjectValue objectId) = ObjectValue $ J.ObjectReference objectId

-- }}}

-- StackFrame functions section {{{

{- | The state of one method invocation on a thread's call stack. As a thread
executes, stack frames are pushed and popped from its call stack as methods are
invoked and then return. A 'StackFrame' mirrors one such frame from a target VM at
some point in its thread's execution. The call stack is, then, simply a list of
'StackFrame' objects. The call stack can be obtained any time a thread is
suspended through a call to 'Language.Java.Jdi.ThreadReference.allFrames'
function of 'ThreadReference'.

'StackFrame's provide access to a method's local variables and their current
values.

The lifetime of a 'StackFrame' is very limited. It is available only for
suspended threads and becomes invalid once its thread is resumed.
-}
data StackFrame = StackFrame ThreadReference J.StackFrame
                  deriving (Eq, Show)

-- }}}

-- ThreadReference functions section {{{

{- | A thread object from the target VM. A ThreadReference is an ObjectReference
with additional access to thread-specific information from the target VM.
-}
data ThreadReference = ThreadReference String J.JavaThreadId -- name threadId
                       deriving (Eq, Show)

threadReferenceFromId :: (Error e, MonadIO m, MonadError e m)
                      => J.JavaThreadId -> VirtualMachine m ThreadReference
threadReferenceFromId refId = do
    reply <- runCommand $ J.threadReferenceNameCommand refId
    let r = J.dat reply
    let name = runGet J.parseString (J.toLazy r)
    return $ ThreadReference name refId

{- | Returns a List containing each StackFrame in the thread's current call
stack. The thread must be suspended (normally through an interruption to the VM)
to get this information, and it is only valid until the thread is resumed again.

Returns: a list of StackFrame with the current frame first followed by each
caller's frame.
-}
allFrames :: (Error e, MonadIO m, MonadError e m)
          => ThreadReference -> VirtualMachine m [StackFrame]
allFrames tr = getFrames tr 0 (-1)

{- | Returns the number of stack frames in the thread's current call stack. The
thread must be suspended (normally through an interruption to the VM) to get
this information, and it is only valid until the thread is resumed again.

Returns: an integer frame count
-}
frameCount :: (Error e, MonadIO m, MonadError e m)
           => ThreadReference -> VirtualMachine m Int
frameCount tr@(ThreadReference _ tId) = do
    reply <- runCommand $ J.frameCountCommand tId
    let count = runGet J.parseInt (J.toLazy $ J.dat reply)
    return $ fromIntegral count

{- | Returns a List containing a range of StackFrame mirrors from the thread's
current call stack. The thread must be suspended (normally through an
interruption to the VM) to get this information, and it is only valid until the
thread is resumed again.

Returns:
    a List of StackFrame with the current frame first followed by each caller's frame.
-}
frames :: (Error e, MonadIO m, MonadError e m) =>
          ThreadReference
          -> Int -- ^ start - the index of the first frame to retrieve.
                 -- Index 0 represents the current frame.
          -> Int -- ^ length - the number of frames to retrieve
          -> VirtualMachine m [StackFrame]
frames tr start len = do
    when (start < 0) $ throwError $ strMsg "negative start"
    when (len < 0) $ throwError $ strMsg "negative len"
    getFrames tr start len

frame :: (Error e, MonadIO m, MonadError e m) =>
         ThreadReference -> Int -> VirtualMachine m StackFrame
frame tr index = do
    head `liftM` getFrames tr index 1

-- This is unsafe implementation of frames command.
getFrames :: (Error e, MonadIO m, MonadError e m)
          => ThreadReference -> Int -> Int -> VirtualMachine m [StackFrame]
getFrames tr@(ThreadReference _ ti) start len = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.framesCommand ti (fromIntegral start) (fromIntegral len)
    let r = J.dat reply
    return $ map (StackFrame tr) $ runGet (J.parseStackFrameList idsizes) (J.toLazy r)

-- | Returns this thread's thread group.
threadGroup :: (Error e, MonadIO m, MonadError e m)
            => ThreadReference -> VirtualMachine m ThreadGroupReference
threadGroup tr@(ThreadReference _ ti) = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.threadGroupCommand ti
    let r = J.dat reply
    let groupId = runGet
                    (J.parseThreadGroupId $ J.threadGroupIdSize idsizes)
                    (J.toLazy r)
    threadGroupReferenceFromId groupId

{- | Returns the thread's status. If the thread is not suspended the thread's
current status is returned. If the thread is suspended, the thread's status
before the suspension is returned is not available. isSuspended() can be used
to determine if the thread has been suspended.
-}
status :: (Error e, MonadIO m, MonadError e m)
       => ThreadReference -> VirtualMachine m J.ThreadStatus
status tr@(ThreadReference _ ti) = do
    reply <- runCommand $ J.threadStatusCommand ti
    let r = J.dat reply
    let (threadStatus, _) = runGet J.parseThreadStatusReply (J.toLazy r)
    return threadStatus

-- | Determines whether the thread has been suspended by the the debugger.
isSuspended :: (Error e, MonadIO m, MonadError e m)
            => ThreadReference -> VirtualMachine m Bool
isSuspended tr@(ThreadReference _ ti) = do
    reply <- runCommand $ J.threadStatusCommand ti
    let r = J.dat reply
    let (_, suspendStatus) = runGet J.parseThreadStatusReply (J.toLazy r)
    return $ suspendStatus == J.Suspended

-- }}}

-- ThreadGroupReference functions section {{{

{- | A thread group object from the target VM. A 'ThreadGroupReference' is an
'J.ObjectReference' with additional access to threadgroup-specific information
from the target VM.
-}
data ThreadGroupReference = ThreadGroupReference String J.JavaThreadGroupId
                            deriving (Eq, Show)

threadGroupReferenceFromId :: (Error e, MonadIO m, MonadError e m)
                           => J.JavaThreadGroupId
                           -> VirtualMachine m ThreadGroupReference
threadGroupReferenceFromId refId = do
    reply <- runCommand $ J.threadGroupReferenceNameCommand refId
    let r = J.dat reply
    let name = runGet J.parseString (J.toLazy r)
    return $ ThreadGroupReference name refId

-- }}}

-- ObjectReference functions section {{{

{- | Prevents garbage collection for this object. By default all ObjectReference
values returned by JDI may be collected at any time the target VM is running.
A call to this method guarantees that the object will not be collected.
enableCollection() can be used to allow collection once again.

Calls to this method are counted. Every call to this method requires a
corresponding call to enableCollection() before garbage collection is
re-enabled.

Note that while the target VM is suspended, no garbage collection will occur
because all threads are suspended. The typical examination of variables, fields,
and arrays during the suspension is safe without explicitly disabling garbage
collection.

This method should be used sparingly, as it alters the pattern of garbage
collection in the target VM and, consequently, may result in application
behavior under the debugger that differs from its non-debugged behavior.
-}
disableCollection :: (Error e, MonadIO m, MonadError e m)
                  => J.ObjectReference -> VirtualMachine m ()
disableCollection (J.ObjectReference refId) = do
    reply <- runCommand $ J.disableCollectionCommand refId
    return ()

{- | Permits garbage collection for this object. By default all ObjectReference
values returned by JDI may be collected at any time the target VM is running.
A call to this method is necessary only if garbage collection was previously
disabled with disableCollection().
-}
enableCollection :: (Error e, MonadIO m, MonadError e m)
                 => J.ObjectReference -> VirtualMachine m ()
enableCollection (J.ObjectReference refId) = do
    reply <- runCommand $ J.enableCollectionCommand refId
    return ()

{- | Returns the number times this object's monitor has been entered by the
current owning thread. See ownedMonitors function of ThreadReference
for a definition of ownership.

Not all target VMs support this operation. See canGetMonitorInfo to determine
if the operation is supported.

Returns: the integer count of the number of entries.
-}
entryCount :: (Error e, MonadIO m, MonadError e m)
           => J.ObjectReference -> VirtualMachine m Int
entryCount (J.ObjectReference refId) = do
    reply <- runCommand $ J.monitorInfoCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let (_, count, _) = runGet (J.parseMonitorInfoReply idsizes) (J.toLazy r)
    return $ fromIntegral count

invokeMethod :: J.ObjectReference
             -> ThreadReference
             -> Method
             -> [Value]
             -> Int
             -> Value
invokeMethod = undefined

isCollected :: J.ObjectReference -> VirtualMachine m Bool
isCollected = undefined

owningThread :: J.ObjectReference -> VirtualMachine m ThreadReference
owningThread = undefined

referringObjects :: J.ObjectReference
                 -> Int
                 -> VirtualMachine m [J.ObjectReference]
referringObjects = undefined

setValue :: J.ObjectReference
         -> Field
         -> Value
         -> VirtualMachine m ()
setValue = undefined

uniqueId :: J.ObjectReference
         -> VirtualMachine m Int
uniqueId = undefined

waitingThreads :: J.ObjectReference -> VirtualMachine m [ThreadReference]
waitingThreads = undefined

-- }}}

-- Field functions section {{{

{- | A class or instance variable in the target VM. See TypeComponent
for general information about Field and Method mirrors.
-}
data Field = Field J.ReferenceType J.Field
              deriving (Eq, Show)

-- }}}

-- Method functions section {{{

{- | A static or instance method in the target VM. See TypeComponent for general
information about Field and Method mirrors. -}
data Method = Method J.ReferenceType J.Method
              deriving (Eq, Show)

methodAllLineLocations :: (Error e, MonadIO m, MonadError e m)
                        => Method -> VirtualMachine m [Location]
methodAllLineLocations m@(Method ref method) = do
        (J.LineTable _ _ lines) <- receiveLineTable m
        return $ map (Location ref method) lines

-- }}}

-- LocalVariable functions section {{{

{- | A local variable in the target VM. Each variable declared within a Method
has its own LocalVariable object. Variables of the same name declared in
different scopes have different LocalVariable objects. LocalVariables can be
used alone to retrieve static information about their declaration, or can be
used in conjunction with a StackFrame to set and get values.
-}
data LocalVariable = LocalVariable J.ReferenceType J.Method J.Slot
                     deriving (Show, Eq)

localVariableName :: LocalVariable -> String
localVariableName (LocalVariable _ _ (J.Slot _ nm _ _ _)) = nm

-- }}}

-- Location functions section {{{

{- | A point within the executing code of the target VM. Locations are used to
identify the current position of a suspended thread (analogous to an instruction
pointer or program counter register in native programs). They are also used to
identify the position at which to set a breakpoint.

The availability of a line number for a location will depend on the level of
debugging information available from the target VM.

Several mirror interfaces have locations. Each such mirror extends a Locatable
interface.

Strata

The source information for a Location is dependent on the stratum which is used.
A stratum is a source code level within a sequence of translations. For example,
say the baz program is written in the programming language "Foo" then translated
to the language "Bar" and finally translated into the Java programming language.
The Java programming language stratum is named "Java", let's say the other
strata are named "Foo" and "Bar". A given location (as viewed by the
sourceName() and lineNumber() methods) might be at line 14 of "baz.foo" in the
"Foo" stratum, line 23 of "baz.bar" in the "Bar" stratum and line 71 of the
"Java" stratum. Note that while the Java programming language may have only one
source file for a reference type, this restriction does not apply to other
strata - thus each Location should be consulted to determine its source path.
Queries which do not specify a stratum (sourceName(), sourcePath() and
lineNumber()) use the VM's default stratum (VirtualMachine.getDefaultStratum()).
If the specified stratum (whether explicitly specified by a method parameter or
implicitly as the VM's default) is null or is not available in the declaring
type, the declaring type's default stratum is used
(declaringType().defaultStratum()). Note that in the normal case, of code that
originates as Java programming language source, there will be only one stratum
("Java") and it will be returned as the default. To determine the available
strata use ReferenceType.availableStrata().

Only default Java stratum is available in the current JDI version.
-}

data Location = Location J.ReferenceType J.Method J.Line
                deriving (Show, Eq)

{- | Gets the code position within this location's method.

Returns: the Int representing the position within the method or -1 if location
is within a native method.
-}
codeIndex :: Location -> Int
codeIndex (Location _ _ (J.Line ci _)) = fromIntegral ci

locationDeclaringType :: Location -> J.ReferenceType
locationDeclaringType (Location rt _ _) = rt

{- | Gets the line number of this Location.

This method is equivalent to lineNumber(vm.getDefaultStratum()) - see
lineNumber(String) for more information.

Returns: an Int specifying the line in the source, returns -1 if the information
is not available; specifically, always returns -1 for native methods.
-}
lineNumber :: Location -> Int
lineNumber (Location _ _ (J.Line _ ln)) = fromIntegral ln

-- | Gets the method containing this Location.
method :: Location -> Method
method (Location refType method _) = Method refType method

locationSourceName :: (Error e, MonadIO m, MonadError e m)
                   => Location -> VirtualMachine m String
locationSourceName (Location (J.ReferenceType _ refId _ _) _ _) = sourceNameFromRefId refId

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
                          J.TypeTag -> J.JavaReferenceTypeId -> VirtualMachine m J.ReferenceType
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
    methodList <- methods rt
    let (Just method) = find isMyMethod methodList
    al <- methodAllLineLocations method
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

sourceNameFromRefId :: (Error e , MonadIO m, MonadError e m)
                    => J.JavaReferenceTypeId -> VirtualMachine m String
sourceNameFromRefId refId = do
    reply <- runCommand $ J.sourceFileCommand refId
    let r = J.dat reply
    let sourceName = runGet J.parseString (J.toLazy r)
    return sourceName
-- }}}

-- vim: foldmethod=marker foldmarker={{{,}}}

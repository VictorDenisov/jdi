module Language.Java.Jdi.Impl
( VirtualMachine
, runVirtualMachine
, Name(..)
, Resumable(..)
, Locatable(..)
, SourceName(..)
, AllLineLocations(..)
, RefType(..)
, DeclaringType(..)
, GenericSignature(..)
, Signature(..)
, Accessible(..)
, TypeComponent(..)
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
, suspendVm
, topLevelThreadGroups
, J.Event
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
, refTypeGetValue
, fields
, methods
, interfaces
, superclass
, J.ArrayReference
, getArrValue
, getArrValues
, arrLength
, J.StringReference
, stringValue
, Value(..)
, StackFrame
, stackFrameGetValue
, thisObject
, ThreadReference
, allFrames
, frameCount
, frames
, threadGroup
, status
, isSuspended
, ThreadGroupReference
, parent
, threadGroups
, threads
, J.ObjectReference
, disableCollection
, enableCollection
, entryCount
, objGetValue
, objGetValues
, J.StepSize(..)
, J.StepDepth(..)
, J.ThreadStatus(..)
, J.SuspendStatus(..)
, Field
, Method
, arguments
, variables
, variablesByName
, LocalVariable
, Location
, codeIndex
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
    name :: a -> String

class Resumable a where
    resume :: (Error e, MonadIO m, MonadError e m) => a -> VirtualMachine m ()

class Locatable a where
    location :: (Error e, MonadIO m, MonadError e m) => a -> VirtualMachine m Location

class SourceName a where
    sourceName :: (Error e, MonadIO m, MonadError e m) => a -> VirtualMachine m String

class AllLineLocations a where
    allLineLocations :: (Error e, MonadIO m, MonadError e m) => a -> VirtualMachine m [Location]

class RefType a where
    referenceType :: a -> J.ReferenceType

class DeclaringType a where
    declaringType :: a -> J.ReferenceType

class GenericSignature a where
    genericSignature :: a -> String

class Signature a where
    signature :: a -> String

class Accessible a where
    isPackagePrivate :: a -> Bool
    isPrivate :: a -> Bool
    isProtected :: a -> Bool
    isPublic :: a -> Bool
    modifiers :: a -> Int

class ( Name a
      , DeclaringType a
      , GenericSignature a
      , Accessible a
      , Signature a)
   => TypeComponent a where
    isFinal :: a -> Bool
    isStatic :: a -> Bool
    isSynthetic :: a -> Bool

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

-- | Continues the execution of the application running in this virtual machine.
resumeVm :: (Error e, MonadIO m, MonadError e m) => VirtualMachine m ()
resumeVm = runCommand J.resumeVmCommand >> return ()

{- | Suspends the execution of the application running in this virtual machine.
All threads currently running will be suspended.

Unlike Thread.suspend(), suspends of both the virtual machine and individual
threads are counted. Before a thread will run again, it must be resumed (through
resume() or ThreadReference.resume()) the same number of times it has been
suspended. -}
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
instance Locatable J.Event where
    location (J.BreakpointEvent _ _ javaLocation) =
        locationFromJavaLocation javaLocation
    location (J.StepEvent _ _ javaLocation) =
        locationFromJavaLocation javaLocation

-- | Returns the reference type for which this event was generated.

instance RefType J.Event where
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
                  | StepRequest ThreadReference J.StepSize J.StepDepth
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
                (ThreadReference _ tId) ss sd)) = do
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

createStepRequest :: ThreadReference -> J.StepSize -> J.StepDepth -> EventRequest
createStepRequest tr ss sd = EventRequest J.SuspendAll Nothing [] (StepRequest tr ss sd)

-- }}}

-- ReferenceType functions section {{{

instance Signature J.ReferenceType where
    signature (J.ReferenceType _ _ gs _) = gs

{- | Gets the Value of a given static Field in this type. The Field must be
valid for this type; that is, it must be declared in this type, a superclass,
a superinterface, or an implemented interface.

Program should be started to be sure static fields are properly
initialized.
 -}
refTypeGetValue :: (Error e, MonadIO m, MonadError e m) =>
                   J.ReferenceType -> Field -> VirtualMachine m Value
refTypeGetValue (J.ReferenceType _ ri _ _) field@(Field _ f) = do
    when (not $ isStatic field)
                $ throwError $ strMsg "Only static fields are allowed in ReferenceType getValue"
    reply <- runCommand $ J.refGetValuesCommand ri [f]
    idsizes <- getIdSizes
    return $ toJdiValue $ head $ runGet
                        (J.parseGetValuesReply idsizes)
                        (J.toLazy $ J.dat reply)

{- | Returns a list containing each Field declared in this type. Inherited
fields are not included. Any synthetic fields created by the compiler are
included in the list.

For arrays (ArrayType) and primitive classes, the returned list is always empty.
-}
fields :: (Error e, MonadIO m, MonadError e m) =>
             J.ReferenceType -> VirtualMachine m [Field]
fields rt@(J.ReferenceType _ refId _ _) = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.fieldsCommand refId
    let r = J.dat reply
    let fields = runGet (J.parseFieldsReply idsizes) (J.toLazy r)
    return $ map (Field rt) fields

methods :: (Error e, MonadIO m, MonadError e m) =>
              J.ReferenceType -> VirtualMachine m [Method]
methods rt@(J.ReferenceType _ refId _ _) = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.methodsCommand refId
    let r = J.dat reply
    let methods = runGet (J.parseMethodsReply idsizes) (J.toLazy r)
    return $ map (Method rt) methods

instance Name J.ReferenceType where
    name = signatureToName . signature

instance SourceName J.ReferenceType where
    sourceName (J.ReferenceType _ refId _ _) = do
        reply <- runCommand $ J.sourceFileCommand refId
        let r = J.dat reply
        let sourceName = runGet J.parseString (J.toLazy r)
        return sourceName

instance AllLineLocations J.ReferenceType where
    allLineLocations refType = concat `liftM` ((mapM allLineLocations) =<< (methods refType))

-- For interfaces returns interfaces extended by this interface.
interfaces :: (Error e, MonadIO m, MonadError e m) =>
              J.ReferenceType -> VirtualMachine m [J.ReferenceType]
interfaces (J.ReferenceType _ refId _ _) = do
    reply <- runCommand $ J.interfacesCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let interfaceIds = runGet (J.parseInterfacesReply idsizes) (J.toLazy r)
    mapM (referenceTypeFromRefId J.Interface) interfaceIds

-- | Doesn't work for ReferenceTypes that are interfaces.
superclass (J.ReferenceType _ refId _ _) = do
    reply <- runCommand $ J.superclassCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let subRefId = runGet
                (J.parseReferenceTypeId $ J.referenceTypeIdSize idsizes)
                (J.toLazy r)
    referenceTypeFromRefId J.Class subRefId

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

data StackFrame = StackFrame ThreadReference J.StackFrame
                  deriving (Eq, Show)

stackFrameGetValue :: (Error e, MonadIO m, MonadError e m) =>
            StackFrame -> LocalVariable -> VirtualMachine m Value
stackFrameGetValue (StackFrame (ThreadReference _ ti) (J.StackFrame fi _))
         (LocalVariable _ _ slot) = do
    reply <- runCommand $ J.getValuesCommand ti fi [slot]
    idsizes <- getIdSizes
    return $ toJdiValue $ head $ runGet
                        (J.parseGetValuesReply idsizes)
                        (J.toLazy $ J.dat reply)

instance Locatable StackFrame where
    location (StackFrame _ (J.StackFrame _ javaLoc))
                        = locationFromJavaLocation javaLoc

thisObject :: (Error e, MonadIO m, MonadError e m)
           => StackFrame -> VirtualMachine m J.ObjectReference
thisObject sf@(StackFrame (ThreadReference _ ti) (J.StackFrame fi _)) = do
    loc <- location sf
    let mtd = method loc
    when (isStatic mtd) $
                throwError $ strMsg "Can get this object for static method"
    reply <- runCommand $ J.thisObjectCommand ti fi
    idsizes <- getIdSizes
    let (ObjectValue ref) = toJdiValue $ runGet
                        (J.parseTaggedValue idsizes)
                        (J.toLazy $ J.dat reply)
    return ref

-- }}}

-- ThreadReference functions section {{{

                                    --  name     threadId
data ThreadReference = ThreadReference String J.JavaThreadId
                       deriving (Eq, Show)

threadReferenceFromId :: (Error e, MonadIO m, MonadError e m)
                      => J.JavaThreadId -> VirtualMachine m ThreadReference
threadReferenceFromId refId = do
    reply <- runCommand $ J.threadReferenceNameCommand refId
    let r = J.dat reply
    let name = runGet J.parseString (J.toLazy r)
    return $ ThreadReference name refId

instance Name ThreadReference where
    name (ThreadReference n _) = n

instance Resumable ThreadReference where
    resume (ThreadReference _ tId) = resumeThreadId tId

allFrames :: (Error e, MonadIO m, MonadError e m)
          => ThreadReference -> VirtualMachine m [StackFrame]
allFrames tr = getFrames tr 0 (-1)

frameCount :: (Error e, MonadIO m, MonadError e m)
           => ThreadReference -> VirtualMachine m Int
frameCount tr@(ThreadReference _ tId) = do
    reply <- runCommand $ J.frameCountCommand tId
    let count = runGet J.parseInt (J.toLazy $ J.dat reply)
    return $ fromIntegral count

frames :: (Error e, MonadIO m, MonadError e m) =>
          ThreadReference -> Int -> Int -> VirtualMachine m [StackFrame]
frames tr start len = do
    when (start < 0) $ throwError $ strMsg "negative start"
    when (len < 0) $ throwError $ strMsg "negative len"
    getFrames tr start len

-- This is unsafe implementation of frames command.
getFrames :: (Error e, MonadIO m, MonadError e m)
          => ThreadReference -> Int -> Int -> VirtualMachine m [StackFrame]
getFrames tr@(ThreadReference _ ti) start len = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.framesCommand ti (fromIntegral start) (fromIntegral len)
    let r = J.dat reply
    return $ map (StackFrame tr) $ runGet (J.parseStackFrameList idsizes) (J.toLazy r)

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

status :: (Error e, MonadIO m, MonadError e m)
       => ThreadReference -> VirtualMachine m J.ThreadStatus
status tr@(ThreadReference _ ti) = do
    reply <- runCommand $ J.threadStatusCommand ti
    let r = J.dat reply
    let (threadStatus, _) = runGet J.parseThreadStatusReply (J.toLazy r)
    return threadStatus

isSuspended :: (Error e, MonadIO m, MonadError e m)
            => ThreadReference -> VirtualMachine m Bool
isSuspended tr@(ThreadReference _ ti) = do
    reply <- runCommand $ J.threadStatusCommand ti
    let r = J.dat reply
    let (_, suspendStatus) = runGet J.parseThreadStatusReply (J.toLazy r)
    return $ suspendStatus == J.Suspended

-- }}}

-- ThreadGroupReference functions section {{{

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

instance Name ThreadGroupReference where
    name (ThreadGroupReference n _) = n

parent :: (Error e, MonadIO m, MonadError e m)
       => ThreadGroupReference -> VirtualMachine m ThreadGroupReference
parent (ThreadGroupReference _ refId) = do
    reply <- runCommand $ J.threadGroupReferenceParentCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let parentId = runGet
                    (J.parseThreadGroupId $ J.threadGroupIdSize idsizes)
                    (J.toLazy r)
    threadGroupReferenceFromId parentId

threadGroups :: (Error e, MonadIO m, MonadError e m)
             => ThreadGroupReference
             -> VirtualMachine m [ThreadGroupReference]
threadGroups (ThreadGroupReference _ refId) = do
    reply <- runCommand $ J.threadGroupReferenceChildrenCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let (_, groups) = runGet
                        (J.parseThreadGroupChildrenReply idsizes)
                        (J.toLazy r)
    mapM threadGroupReferenceFromId groups

threads :: (Error e, MonadIO m, MonadError e m)
             => ThreadGroupReference
             -> VirtualMachine m [ThreadReference]
threads (ThreadGroupReference _ refId) = do
    reply <- runCommand $ J.threadGroupReferenceChildrenCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let (ts, _) = runGet
                        (J.parseThreadGroupChildrenReply idsizes)
                        (J.toLazy r)
    mapM threadReferenceFromId ts

-- }}}

-- ObjectReference functions section {{{

disableCollection :: (Error e, MonadIO m, MonadError e m)
                  => J.ObjectReference -> VirtualMachine m ()
disableCollection (J.ObjectReference refId) = do
    reply <- runCommand $ J.disableCollectionCommand refId
    return ()

enableCollection :: (Error e, MonadIO m, MonadError e m)
                 => J.ObjectReference -> VirtualMachine m ()
enableCollection (J.ObjectReference refId) = do
    reply <- runCommand $ J.enableCollectionCommand refId
    return ()

entryCount :: (Error e, MonadIO m, MonadError e m)
           => J.ObjectReference -> VirtualMachine m Int
entryCount (J.ObjectReference refId) = do
    reply <- runCommand $ J.monitorInfoCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let (_, count, _) = runGet (J.parseMonitorInfoReply idsizes) (J.toLazy r)
    return $ fromIntegral count

objGetValue :: (Error e, MonadIO m, MonadError e m)
            => J.ObjectReference -> Field -> VirtualMachine m Value
objGetValue (J.ObjectReference ri) (Field _ f) = do
    reply <- runCommand $ J.objGetValuesCommand ri [f]
    idsizes <- getIdSizes
    return $ toJdiValue $ head $ runGet
                        (J.parseGetValuesReply idsizes)
                        (J.toLazy $ J.dat reply)

objGetValues :: (Error e, MonadIO m, MonadError e m)
             => J.ObjectReference -> [Field] -> VirtualMachine m [Value]
objGetValues (J.ObjectReference ri) fs = do
    reply <- runCommand $ J.objGetValuesCommand ri (map getFid fs)
    idsizes <- getIdSizes
    return $ map toJdiValue $ runGet (J.parseGetValuesReply idsizes)
                             (J.toLazy $ J.dat reply)
    where getFid (Field _ f) = f

invokeMethod :: J.ObjectReference -- | object
             -> ThreadReference -- | thread
             -> Method -- | Method
             -> [Value] -- | Arguments
             -> Int -- | Options
             -> Value -- | return value
invokeMethod = undefined

isCollected :: J.ObjectReference -> VirtualMachine m Bool
isCollected = undefined

owningThread :: J.ObjectReference -> VirtualMachine m ThreadReference
owningThread = undefined

instance RefType J.ObjectReference where
    referenceType = undefined

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

data Field = Field J.ReferenceType J.Field
              deriving (Eq, Show)

instance Name Field  where
    name (Field _ (J.Field _ nm _ _)) = nm

instance Accessible Field where
    isPackagePrivate f = not (isPrivate f)
                      && not (isProtected f)
                      && not (isPublic f)
    isPrivate (Field _ (J.Field _ _ _ modbits))
                                = (modbits .&. J.field_private) /= 0
    isProtected (Field _ (J.Field _ _ _ modbits))
                                = (modbits .&. J.field_protected) /= 0
    isPublic (Field _ (J.Field _ _ _ modbits))
                                = (modbits .&. J.field_public) /= 0
    modifiers (Field _ (J.Field _ _ _ modbits)) = fromIntegral modbits

instance DeclaringType Field where
    declaringType (Field rt _) = rt

-- TODO needs implementation
instance GenericSignature Field where
    genericSignature (Field _ (J.Field _ _ sig _)) = undefined

instance Signature Field where
    signature (Field _ (J.Field _ _ sig _)) = sig

instance TypeComponent Field where
    isFinal (Field _ (J.Field _ _ _ modbits))
                            = (J.field_final .&. modbits) /= 0
    isStatic (Field _ (J.Field _ _ _ modbits))
                            = (J.field_static .&. modbits) /= 0
    isSynthetic (Field _ (J.Field _ _ _ modbits))
                            = (J.field_synthetic .&. modbits) /= 0

-- }}}

-- Method functions section {{{

data Method = Method J.ReferenceType J.Method
              deriving (Eq, Show)

instance Name Method where
    name (Method _ (J.Method _ name _ _)) = name

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
    (filter ((varName ==) . name)) `liftM` (variables method)

instance Accessible Method where
    isPackagePrivate f = not (isPrivate f)
                      && not (isProtected f)
                      && not (isPublic f)
    isPrivate (Method _ (J.Method _ _ _ modbits))
                                = (modbits .&. J.method_private) /= 0
    isProtected (Method _ (J.Method _ _ _ modbits))
                                = (modbits .&. J.method_protected) /= 0
    isPublic (Method _ (J.Method _ _ _ modbits))
                                = (modbits .&. J.method_public) /= 0
    modifiers (Method _ (J.Method _ _ _ modbits)) = fromIntegral modbits

instance DeclaringType Method where
    declaringType (Method rt _) = rt

-- TODO needs implementation
instance GenericSignature Method where
    genericSignature (Method _ (J.Method _ _ sig _)) = undefined

instance Signature Method where
    signature (Method _ (J.Method _ _ sig _)) = sig

instance TypeComponent Method where
    isFinal (Method _ (J.Method _ _ _ modbits))
                            = (J.method_final .&. modbits) /= 0
    isStatic (Method _ (J.Method _ _ _ modbits))
                            = (J.method_static .&. modbits) /= 0
    isSynthetic (Method _ (J.Method _ _ _ modbits))
                            = (J.method_synthetic .&. modbits) /= 0
-- }}}

-- LocalVariable functions section {{{

data LocalVariable = LocalVariable J.ReferenceType J.Method J.Slot
                     deriving (Show, Eq)

instance Name LocalVariable where
    name (LocalVariable _ _ (J.Slot _ nm _ _ _)) = nm

-- }}}

-- Location functions section {{{

data Location = Location J.ReferenceType J.Method J.Line
                deriving (Show, Eq)

codeIndex :: Location -> Int
codeIndex (Location _ _ (J.Line ci _)) = fromIntegral ci

instance DeclaringType Location where
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

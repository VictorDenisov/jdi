module Jdi
( VirtualMachine
, runVirtualMachine
, vmName
, description
, version
, removeEvent
, resumeVm
, EventRequest
, enable
, createClassPrepareRequest
, createBreakpointRequest
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
, ReferenceType
, allClasses
, J.ThreadReference
, allThreads
, classesByName
, exit
, J.ThreadGroupReference
, topLevelThreadGroups
, dispose
, Method
, Name(..)
, allMethods
, Location
, allLineLocations
, location
, J.EventSet(..)
, J.Event
, J.eventKind
, J.SuspendPolicy(..)
) where

import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad (guard, when)
import qualified Jdwp as J
import Network (connectTo, PortID)
import qualified Data.Map as M
import Network.Socket.Internal (PortNumber(..))
import GHC.IO.Handle (Handle, hClose, hSetBinaryMode, hPutStr, hFlush)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import GHC.IO.Handle (hWaitForInput)
import qualified Data.Sequence as S

type VirtualMachine m = StateT Configuration (ErrorT String m)

-- Configuration description
---- {{{
data Configuration = Configuration
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
-- }}}

runVirtualMachine :: MonadIO m =>
                            String -> PortID -> VirtualMachine m () -> m ()
runVirtualMachine host port vm = do
    h <- liftIO $ connectTo host port
    liftIO $ hSetBinaryMode h True
    result <- runErrorT $ runStateT
                            ((lift (J.handshake h)) >> (preflight >> vm >> releaseResources))
                            (initialConfiguration h)
    case result of
        Right ((), state) -> return ()
        Left s -> liftIO $ putStrLn $ "Execution failed with message: " ++ s

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

initialConfiguration :: Handle -> Configuration
initialConfiguration h = Configuration Nothing 0 h S.empty Nothing Nothing

class Name a where
    name :: MonadIO m => a -> VirtualMachine m String

class Resumable a where
    resume :: MonadIO m => a -> VirtualMachine m ()

--- Auxiliary functions
runVersionCommand :: MonadIO m => VirtualMachine m J.Version
runVersionCommand = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.versionCommand cntr
    p <- liftIO $ J.waitReply h
    return $ runGet J.parseVersion (J.toLazy $ J.dat p)
    
receiveLineTable :: MonadIO m => Method -> VirtualMachine m J.LineTable
receiveLineTable (Method (J.ReferenceType _ refId _ _)
                         (J.Method mId _ _ _)) = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.lineTableCommand cntr refId mId
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    return $ runGet J.parseLineTableReply (J.toLazy r)

--- Functions from official interface
-- {{{
vmName :: MonadIO m => VirtualMachine m String
vmName = J.vmName `liftM` runVersionCommand

description :: MonadIO m => VirtualMachine m String
description = J.description `liftM` runVersionCommand

version :: MonadIO m => VirtualMachine m String
version = J.vmVersion `liftM` runVersionCommand

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

resumeVm :: MonadIO m => VirtualMachine m ()
resumeVm = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    idsizes <- getIdSizes
    liftIO $ J.sendPacket h $ J.resumeVmCommand cntr
    r <- liftIO $ J.waitReply h
    return ()

data EventRequest = ClassPrepareRequest EventRequest
                  | BreakpointRequest EventRequest Location
                  | EventRequest J.SuspendPolicy
                  | RequestDescriptor J.EventKind J.JavaInt
                    deriving (Show, Eq)

enable :: MonadIO m => EventRequest -> VirtualMachine m EventRequest
enable (ClassPrepareRequest (EventRequest suspendPolicy)) = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.eventSetRequest cntr J.ClassPrepare suspendPolicy []
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let requestId = runGet J.parseInt (J.toLazy r)
    return $ RequestDescriptor J.ClassPrepare requestId
enable (BreakpointRequest
                (EventRequest suspendPolicy)
                (Location (J.ReferenceType typeTag refId _ _)
                          (J.Method mId _ _ _)
                          (J.Line codeIndex lineNum))) = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    let modifiers = [J.LocationOnly (J.JavaLocation typeTag refId mId codeIndex)]
    let packet = J.eventSetRequest cntr J.Breakpoint suspendPolicy modifiers
    liftIO . putStrLn $ "sent packet: " ++ (show packet)
    liftIO $ J.sendPacket h packet
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    liftIO . putStrLn $ "received packet" ++ (show r)
    let requestId = runGet J.parseInt (J.toLazy r)
    liftIO . putStrLn $ show requestId
    return $ RequestDescriptor J.Breakpoint requestId

createClassPrepareRequest :: EventRequest
createClassPrepareRequest = ClassPrepareRequest $ EventRequest J.SuspendAll

createBreakpointRequest :: Location -> EventRequest
createBreakpointRequest = BreakpointRequest $ EventRequest J.SuspendAll

canAddMethod :: MonadIO m => VirtualMachine m Bool
canAddMethod = J.canAddMethod `liftM` getCapabilities

canBeModified :: MonadIO m => VirtualMachine m Bool
canBeModified = undefined -- I'm not sure what should be here right now.
                          -- Different JDI implementations put just constant
                          -- values true or false. TODO

canGetBytecodes :: MonadIO m => VirtualMachine m Bool
canGetBytecodes = J.canGetBytecodes `liftM` getCapabilities

canGetCurrentContendedMonitor :: MonadIO m => VirtualMachine m Bool
canGetCurrentContendedMonitor = J.canGetCurrentContendedMonitor `liftM` getCapabilities

canGetMonitorInfo :: MonadIO m => VirtualMachine m Bool
canGetMonitorInfo = J.canGetMonitorInfo `liftM` getCapabilities

canGetOwnedMonitorInfo :: MonadIO m => VirtualMachine m Bool
canGetOwnedMonitorInfo = J.canGetOwnedMonitorInfo `liftM` getCapabilities

canGetSourceDebugExtension :: MonadIO m => VirtualMachine m Bool
canGetSourceDebugExtension = J.canGetSourceDebugExtension `liftM` getCapabilities

canGetSynteticAttribute :: MonadIO m => VirtualMachine m Bool
canGetSynteticAttribute = J.canGetSynteticAttribute `liftM` getCapabilities

canPopFrames :: MonadIO m => VirtualMachine m Bool
canPopFrames = J.canPopFrames `liftM` getCapabilities

canRedefineClasses :: MonadIO m => VirtualMachine m Bool
canRedefineClasses = J.canRedefineClasses `liftM` getCapabilities

canRequestVmDeathEvent :: MonadIO m => VirtualMachine m Bool
canRequestVmDeathEvent = J.canRequestVmDeathEvent `liftM` getCapabilities

canUnrestrictedlyRedefineClasses :: MonadIO m => VirtualMachine m Bool
canUnrestrictedlyRedefineClasses = J.canUnrestrictedlyRedefineClasses `liftM` getCapabilities

canUseInstanceFilters :: MonadIO m => VirtualMachine m Bool
canUseInstanceFilters = J.canUseInstanceFilters `liftM` getCapabilities

canWatchFieldAccess :: MonadIO m => VirtualMachine m Bool
canWatchFieldAccess = J.canWatchFieldAccess `liftM` getCapabilities

canWatchFieldModification :: MonadIO m => VirtualMachine m Bool
canWatchFieldModification = J.canWatchFieldModification `liftM` getCapabilities

type ReferenceType = J.ReferenceType

allClasses :: MonadIO m => VirtualMachine m [ReferenceType]
allClasses = do
    h <- getVmHandle
    idsizes <- getIdSizes
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.allClassesCommand cntr
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let classes = runGet (J.parseAllClassesReply idsizes) (J.toLazy r)
    return classes

instance Name J.ThreadReference where
    name (J.ThreadReference refId) = do
        h <- getVmHandle
        cntr <- yieldPacketIdCounter
        liftIO $ J.sendPacket h $ J.threadReferenceNameCommand cntr refId
        r <- J.dat `liftM` (liftIO $ J.waitReply h)
        let name = runGet J.parseString (J.toLazy r)
        return name

resumeThreadId :: MonadIO m => J.JavaThreadId -> VirtualMachine m ()
resumeThreadId tId = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.resumeThreadCommand cntr tId
    r <- liftIO $ J.waitReply h
    return ()

instance Resumable J.ThreadReference where
    resume (J.ThreadReference tId) = resumeThreadId tId

allThreads :: MonadIO m => VirtualMachine m [J.ThreadReference]
allThreads = do
    h <- getVmHandle
    idsizes <- getIdSizes
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.allThreadsCommand cntr
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let threads = runGet (J.parseAllThreadsReply idsizes) (J.toLazy r)
    return threads

classesByName :: MonadIO m => String -> VirtualMachine m [ReferenceType]
classesByName name = do
    let jniName = "L" ++ (map replaceDot name) ++ ";"
    h <- getVmHandle
    idsizes <- getIdSizes
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.classesBySignatureCommand cntr jniName
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let classes = runGet (J.parseClassesBySignatureReply idsizes) (J.toLazy r)
    return $ map (setSignature jniName) classes
    
    where
        replaceDot '.' = '/'
        replaceDot x = x
        setSignature newSig (J.ReferenceType tt ri _ cs) =
            J.ReferenceType tt ri newSig cs

exit :: MonadIO m => Int -> VirtualMachine m ()
exit exitCode = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.exitCommand cntr (fromIntegral exitCode)
    liftIO $ J.waitReply h
    return ()

topLevelThreadGroups :: MonadIO m => VirtualMachine m [J.ThreadGroupReference]
topLevelThreadGroups = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    idsizes <- getIdSizes
    liftIO $ J.sendPacket h $ J.topLevelThreadGroupsCommand cntr
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let groups = runGet (J.parseThreadGroupsReply idsizes) (J.toLazy r)
    return groups

dispose :: MonadIO m => VirtualMachine m ()
dispose = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    liftIO $ J.sendPacket h $ J.disposeCommand cntr
    liftIO $ J.waitReply h
    return ()

data Method = Method J.ReferenceType J.Method
              deriving (Eq, Show)

instance Name Method where
    name (Method _ (J.Method _ name _ _)) = return name

allMethods :: MonadIO m => ReferenceType -> VirtualMachine m [Method]
allMethods rt@(J.ReferenceType _ refId _ _) = do
    h <- getVmHandle
    cntr <- yieldPacketIdCounter
    idsizes <- getIdSizes
    liftIO $ J.sendPacket h $ J.methodsCommand cntr refId
    r <- J.dat `liftM` (liftIO $ J.waitReply h)
    let methods = runGet (J.parseMethodsReply idsizes) (J.toLazy r)
    return $ map (Method rt) methods

data Location = Location J.ReferenceType J.Method J.Line
                deriving (Show, Eq)

allLineLocations :: MonadIO m => Method -> VirtualMachine m [Location]
allLineLocations m@(Method ref method) = do
    (J.LineTable _ _ lines) <- receiveLineTable m
    return $ map (Location ref method) lines

location :: MonadIO m => Method -> VirtualMachine m Location
location m@(Method ref method) = do
    (J.LineTable _ _ lines) <- receiveLineTable m
    return $ Location ref method (head lines)

instance Resumable J.EventSet where
    resume (J.EventSet J.SuspendAll _) = resumeVm
    resume (J.EventSet J.SuspendEventThread events)
                                        = resumeThreadId $ J.threadId $ head events
    resume (J.EventSet J.SuspendNone _) = return ()
-- }}}
-- vim: foldmethod=marker foldmarker={{{,}}}

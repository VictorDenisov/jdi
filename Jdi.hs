module Jdi where

import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad (guard, when)
import Jdwp
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
    { idSizesConf     :: IdSizes
    , packetIdCounter :: PacketId
    , vmHandle        :: Handle
    , replyParsers    :: M.Map PacketId ReplyDataParser
    , eventQueue      :: S.Seq EventSet
    }

getPacketIdCounter :: Monad m => VirtualMachine m PacketId
getPacketIdCounter = packetIdCounter `liftM` get

incPacketIdCounter :: Monad m => VirtualMachine m ()
incPacketIdCounter = do
    s <- get
    put $ s { packetIdCounter = (packetIdCounter s) + 1 }

addToQueue :: Monad m => EventSet -> VirtualMachine m ()
addToQueue e = do
    s <- get
    put $ s { eventQueue = (eventQueue s) S.|> e}

takeFromQueue :: Monad m => VirtualMachine m EventSet
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

getIdSizes :: Monad m => VirtualMachine m IdSizes
getIdSizes = liftM idSizesConf get

setIdSizes :: Monad m => IdSizes -> VirtualMachine m ()
setIdSizes iss = do
    s <- get
    put $ s { idSizesConf = iss }

-- }}}

runVirtualMachine :: MonadIO m =>
                            String -> PortID -> VirtualMachine m () -> m ()
runVirtualMachine host port vm = do
    h <- liftIO $ connectTo host port
    liftIO $ hSetBinaryMode h True
    result <- runErrorT $ runStateT
                            ((lift (handshake h)) >> (preflight >> vm >> releaseResources))
                            (initialConfiguration h)
    case result of
        Right ((), state) -> return ()
        Left s -> liftIO $ putStrLn $ "Execution failed with message: " ++ s

preflight :: MonadIO m => VirtualMachine m ()
preflight = do
    h <- getVmHandle
    EventSetData vmStartEventSet <- dat `liftM` (liftIO $ waitVmStartEvent h)
    addToQueue vmStartEventSet
    askIdSizes

askIdSizes :: MonadIO m => VirtualMachine m ()
askIdSizes = do
    h <- getVmHandle
    cntr <- getPacketIdCounter
    incPacketIdCounter
    idsizes <- getIdSizes
    liftIO $ sendPacket h idsizes $ idSizesCommand cntr
    let r = liftIO $ waitReply h idsizes $ \_ -> parseIdSizesReply idsizes
    (IdSizesReply newIdSizes) <- dat `liftM` r
    setIdSizes newIdSizes


releaseResources :: MonadIO m => VirtualMachine m ()
releaseResources = do
    s <- get
    liftIO $ hClose $ vmHandle s

initialConfiguration :: Handle -> Configuration
initialConfiguration h = Configuration (IdSizes 0 0 0 0 0) 0 h M.empty S.empty

--- Functions from official interface

name :: MonadIO m => VirtualMachine m String
name = do
    h <- getVmHandle
    cntr <- getPacketIdCounter
    incPacketIdCounter
    idsizes <- getIdSizes
    liftIO $ sendPacket h idsizes $ versionCommand cntr
    p <- liftIO $ waitReply h idsizes $ \_ -> parseVersionReply idsizes
    return $ vmName $ dat p

removeEvent :: MonadIO m => VirtualMachine m EventSet
removeEvent = do
    h <- getVmHandle
    idsizes <- getIdSizes
    qe <- queueEmpty
    if qe
    then do
        EventSetData e <- dat `liftM` (liftIO $ waitEvent h idsizes)
        return e
    else takeFromQueue

resume :: MonadIO m => VirtualMachine m ()
resume = do
    h <- getVmHandle
    cntr <- getPacketIdCounter
    incPacketIdCounter
    idsizes <- getIdSizes
    liftIO $ sendPacket h idsizes $ resumeVmCommand cntr
    r <- liftIO $ waitReply h idsizes $ \_ -> parseEmptyData idsizes
    return ()

data EventRequest = ClassPrepareRequest EventRequest
                  | EventRequest SuspendPolicy
                  | RequestDescriptor EventKind JavaInt
                    deriving (Show, Eq)

enable :: MonadIO m => EventRequest -> VirtualMachine m EventRequest
enable (ClassPrepareRequest (EventRequest suspendPolicy)) = do
    h <- getVmHandle
    idsizes <- getIdSizes
    cntr <- getPacketIdCounter
    incPacketIdCounter
    liftIO $ sendPacket h idsizes $ eventSetRequest cntr ClassPrepare suspendPolicy []
    let r = liftIO $ waitReply h idsizes $ \_ -> parseEventSetRequestReply idsizes
    (EventRequestSetReply requestId) <- dat `liftM` r
    return $ RequestDescriptor ClassPrepare requestId

createClassPrepareRequest :: EventRequest
createClassPrepareRequest = ClassPrepareRequest $ EventRequest SuspendAll

-- vim: foldmethod=marker foldmarker={{{,}}}

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
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import GHC.IO.Handle (hWaitForInput)

type VirtualMachine m = StateT Configuration (ErrorT String m)

-- Configuration description
---- {{{
data Configuration = Configuration
    { idSizesConf     :: IdSizes
    , packetIdCounter :: PacketId
    , vmHandle        :: Handle
    , replyParsers    :: M.Map PacketId ReplyDataParser
    }

getPacketIdCounter :: Monad m => VirtualMachine m PacketId
getPacketIdCounter = liftM packetIdCounter get

incPacketIdCounter :: Monad m => VirtualMachine m ()
incPacketIdCounter = do
    s <- get
    put $ s { packetIdCounter = (packetIdCounter s) + 1 }
 
setIdSizes :: Monad m => IdSizes -> VirtualMachine m ()
setIdSizes iss = do
    s <- get
    put $ s { idSizesConf = iss }

setVmHandle :: Monad m => Handle -> VirtualMachine m ()
setVmHandle h = do
    s <- get
    put $ s { vmHandle = h}

getIdSizes :: Monad m => VirtualMachine m IdSizes
getIdSizes = liftM idSizesConf get
-- }}}

runVirtualMachine :: MonadIO m =>
                            String -> PortID -> VirtualMachine m () -> m ()
runVirtualMachine host port vm = do
    h <- liftIO $ connectTo host port
    liftIO $ hSetBinaryMode h True
    result <- runErrorT $ runStateT
                            ((lift (handshake h)) >> (vm >> releaseResources))
                            (initialConfiguration h)
    case result of
        Right ((), state) -> return ()
        Left s -> liftIO $ putStrLn $ "Execution failed with message: " ++ s

releaseResources :: MonadIO m => VirtualMachine m ()
releaseResources = do
    s <- get
    liftIO $ hClose $ vmHandle s

initialConfiguration :: Handle -> Configuration
initialConfiguration h = Configuration (IdSizes 0 0 0 0 0) 0 h M.empty

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
    then do putStrLn "Receiving a packet..."
            lengthString <- B.hGet h 4
            let length = (fromIntegral $ runGet (parseInt) lengthString) - 4
            reminder <- B.hGet h length
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
            let length = (fromIntegral $ runGet (parseInt) lengthString) - 4
            reminder <- B.hGet h length
            let threadIdSize = fromIntegral $ (length + 4) - 21
            let replyParser = \_ -> error "ReplyDataParser is not expected to be invoked."
            let p = runGet (parsePacket (IdSizes 0 0 threadIdSize 0 0) replyParser) (lengthString `B.append` reminder)
            return p
    else error "No input available where expected"
    

sendPacket :: Handle -> Packet -> IO ()
sendPacket h p = do
    B.hPut h $ runPut $ putPacket p
    hFlush h
-- vim: foldmethod=marker foldmarker={{{,}}}

import Jdi

import Network.Socket.Internal (PortNumber(..))
import Network
import Control.Monad.Trans (liftIO, lift)

main = do
    runVirtualMachine "localhost" (PortNumber 2044) $ do
        nm <- name
        liftIO $ putStrLn $ "Processing virtual ----- machine with name: " ++ nm
        es <- removeEvent
        liftIO $ putStrLn $ show es
        rd <- enable createClassPrepareRequest
        liftIO $ putStrLn $ show rd
        resume
        es <- removeEvent
        liftIO $ putStrLn $ show es
        resume
        es <- removeEvent
        liftIO $ putStrLn $ show es

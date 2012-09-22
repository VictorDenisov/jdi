import Jdi

import Network.Socket.Internal (PortNumber(..))
import Network
import Control.Monad.Trans (liftIO, lift)
import Data.List

main = do
    runVirtualMachine "localhost" (PortNumber 2044) $ do
        vs <- runVersionCommand
        liftIO $ putStrLn $ "Processing virtual ----- machine with name: " ++ (show vs)
        jdv <- getJdwpVersion
        liftIO $ putStrLn $ "JdwpVersion: " ++ (show jdv)
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
        classes <- allClasses
        liftIO $ putStrLn $ intercalate "\n" (map show classes)

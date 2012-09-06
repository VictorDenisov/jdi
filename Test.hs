import Jdi

import Network.Socket.Internal (PortNumber(..))
import Network
import Control.Monad.Trans (liftIO, lift)

main = do
    runVirtualMachine "localhost" (PortNumber 2044) $ do
        liftIO $ putStrLn "Processing virtual ----- machine"

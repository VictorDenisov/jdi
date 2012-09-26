module Main where
import Jdi
import qualified Jdwp as J

import Network.Socket.Internal (PortNumber(..))
import Network
import Control.Monad.Trans (liftIO, lift)
import Control.Applicative ((<$>))
import Data.List

main = do
    runVirtualMachine "localhost" (PortNumber 2044) $ do
        vs <- runVersionCommand
        liftIO . putStrLn $ "Processing virtual ----- machine with name: " ++ (show vs)
        jdv <- getJdwpVersion
        liftIO . putStrLn $ "JdwpVersion: " ++ (show jdv)
        es <- removeEvent
        liftIO . putStrLn $ show es
        rd <- enable createClassPrepareRequest
        liftIO . putStrLn $ show rd

        mainClass <- pollEvents

        classes <- allClasses
        liftIO . putStrLn $ intercalate "\n" (map show classes)
        threads <- allThreads
        liftIO . putStrLn $ intercalate "\n" (map show threads)
        filteredClasses <- classesByName "java.io.BufferedReader"
        liftIO . putStrLn $ intercalate "\n" (map show filteredClasses)
        threadGroups <- topLevelThreadGroups
        liftIO . putStrLn $ intercalate "\n" (map show threadGroups)
        let mainClass = head $ filter isMainClass classes
        methods <- allMethods mainClass
        liftIO . putStrLn $ "Methods for class " ++ (show mainClass)
        liftIO . putStrLn $ intercalate "\n" (map show methods)
        liftIO . putStrLn =<< vmName
        dispose

pollEvents = do
    resume
    es <- removeEvent
    liftIO $ putStrLn $ show es
    if isMainPrepareEvent (head $ J.events es)
    then return ()
    else pollEvents

isMainPrepareEvent (J.ClassPrepareEvent _ _ _ _ "LMain;" _) = True
isMainPrepareEvent _ = False

isMainClass (J.ReferenceType _ _ "LMain;" _) = True
isMainClass _ = False

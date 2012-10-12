module Main where
import Jdi
import qualified Jdwp as J

import Network.Socket.Internal (PortNumber(..))
import Network
import Control.Monad.Trans (liftIO, lift)
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.List

main = do
    runVirtualMachine "localhost" (PortNumber 2044) $ do
        jdv <- version
        liftIO . putStrLn $ "JdwpVersion: " ++ (show jdv)
        es <- removeEvent
        liftIO . putStrLn $ show $ case suspendPolicy es of
                                        SuspendAll -> "this is suspend all"
                                        SuspendNone -> "this is suspend none"
                                        SuspendEventThread -> "this is suspend event thread"
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
        liftIO . putStrLn =<< (name $ head methods)
        forM_ threads (\thread -> liftIO . putStrLn =<< name thread)
        let methodMain = last methods
        lineTable <- allLineLocations methodMain
        mainLocation <- location methodMain
        liftIO . putStrLn $ intercalate "\n" (map show lineTable)
        bpr <- enable $ createBreakpointRequest mainLocation

        pollEvents

        --dispose

pollEvents = do
    resume
    es <- removeEvent
    liftIO $ putStrLn $ show es
    if isMainPrepareEvent (head $ events es)
        then return ()
        else pollEvents

isMainPrepareEvent (J.Event _ _ (J.ClassPrepareEvent _ _ _ "LMain;" _)) = True
isMainPrepareEvent _ = False

isMainClass (J.ReferenceType _ _ "LMain;" _) = True
isMainClass _ = False

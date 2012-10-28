module Main where
import Language.Java.Jdi

import Network.Socket.Internal (PortNumber(..))
import Network
import Control.Monad.Trans (liftIO, lift)
import Control.Applicative ((<$>))
import Control.Monad (forM_, filterM)
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
        cNames <- mapM name classes
        liftIO . putStrLn $ intercalate "\n" cNames
        threads <- allThreads
        liftIO . putStrLn $ intercalate "\n" (map show threads)
        filteredClasses <- classesByName "java.io.BufferedReader"
        liftIO . putStrLn $ intercalate "\n" (map show filteredClasses)
        threadGroups <- topLevelThreadGroups
        liftIO . putStrLn $ intercalate "\n" (map show threadGroups)
        let mainClass = head $ filter isMainClass classes
        sName <- sourceName mainClass
        liftIO . putStrLn $ "Main class source name: " ++ sName
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
        classLineLocations <- allLineLocations mainClass
        liftIO . putStrLn $ intercalate "\n" (map show classLineLocations)
        bpr <- enable $ createBreakpointRequest mainLocation

        pollEvents
        lift . putStrLn $ "Exiting"

        --dispose

pollEvents = do
    resumeVm
    es <- removeEvent
    liftIO $ putStrLn $ show es
    let e = head $ events es
    case eventKind e of
        ClassPrepare -> if isMainClass $ referenceType e
                            then return ()
                            else pollEvents
        VmDeath -> return ()
        _ -> pollEvents

isMainClass ref = "LMain;" == genericSignature ref

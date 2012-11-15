module Main where
import Language.Java.Jdi

import Network.Socket.Internal (PortNumber(..))
import Network
import Control.Monad.Trans (liftIO, lift)
import Control.Applicative ((<$>))
import Control.Monad (forM_, filterM, void, liftM)
import Control.Monad.Error (MonadError(..))
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

        pollEvents $ \e -> case eventKind e of
            ClassPrepare -> isMainClass $ referenceType e
            _ -> False

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
        let isMainMethod = (liftM ("main" ==)) . name
        methodMain <- head <$> (filterM isMainMethod methods)
        let isAnotherMethod = (liftM ("anotherMethod" ==)) . name
        anotherMethod <- head <$> (filterM isAnotherMethod methods)
        liftIO . putStrLn $ "Variables of method anotherMethod: " ++ (show anotherMethod)
        (arguments anotherMethod >>= \l -> liftIO $ putStrLn $ show l)
            `catchError`
                (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))
        liftIO . putStrLn $ "Variables of method main: " ++ (show methodMain)
        (variables methodMain >>= \l -> liftIO $ putStrLn $ show l)
            `catchError`
                (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))

        liftIO . putStrLn $ "VariablesByName"
        (variablesByName methodMain "i" >>= \l -> liftIO $ putStrLn $ show l)
            `catchError`
                (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))
        liftIO . putStrLn $ "Arguments of the method main"
        (arguments methodMain >>= \l -> liftIO $ putStrLn $ show l)
            `catchError`
                (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))
        liftIO . putStrLn $ "Printing line table"
        lineTable <- allLineLocations methodMain
        liftIO . putStrLn $ "After line table"
        mainLocation <- location methodMain
        liftIO . putStrLn $ intercalate "\n" (map show lineTable)
        classLineLocations <- allLineLocations mainClass
        liftIO . putStrLn $ intercalate "\n" (map show classLineLocations)
        liftIO . putStrLn $ "Enabling breakpoint request"
        bpr <- enable $ createBreakpointRequest mainLocation
        liftIO . putStrLn $ "breakpoint request is enabled"
        ev <- pollEvents $ \e -> case eventKind e of
            Breakpoint -> True
            _ -> False
        liftIO . putStrLn $ "breakpoint stopped at location"
        liftIO . putStrLn $ show methodMain
        loc <- location ev
        liftIO . putStrLn $ show loc
        spr <- enable $ (createStepRequest (thread ev) StepLine StepOver)
        resumeVm
        void $ removeEvent
        resumeVm
        void $ removeEvent
        --resumeVm
        --void $ removeEvent

        liftIO . putStrLn $ "trying step requests"
        resumeVm
        es0 <- removeEvent
        let e0 = head $ events es0
        liftIO $ putStrLn $ show e0

        v0 <- getValueOfI $ thread e0
        liftIO $ putStrLn $ show v0

        resumeVm
        void $ removeEvent
        resumeVm
        es1 <- removeEvent
        let e1 = head $ events es1
        liftIO $ putStrLn $ show e1

        v1 <- getValueOfI $ thread e1
        liftIO $ putStrLn $ show v1

        pollEvents $ \e -> case eventKind e of
            VmDeath -> True
            _ -> False
        lift . putStrLn $ "Exiting"

        --dispose

getValueOfI curThread = do
    fr <- head <$> frames curThread 0 0
    liftIO $ putStrLn $ show fr
    loc <- location fr
    liftIO $ putStrLn $ show loc
    var <- head <$> variablesByName (method loc) "i"
    liftIO $ putStrLn $ show var
    getValue fr var

pollEvents stopFunction = do
    resumeVm
    es <- removeEvent
    liftIO $ putStrLn $ show es
    let e = head $ events es
    if stopFunction e
        then return e
        else pollEvents stopFunction

isMainClass ref = "LMain;" == genericSignature ref

module Main where
import Language.Java.Jdi
import qualified Language.Java.Jdi.VirtualMachine as Vm
import qualified Language.Java.Jdi.Event as E
import qualified Language.Java.Jdi.EventSet as ES
import qualified Language.Java.Jdi.EventRequest as ER
import qualified Language.Java.Jdi.ReferenceType as RT

import Network.Socket.Internal (PortNumber(..))
import Network
import Control.Monad.Trans (liftIO, lift)
import Control.Applicative ((<$>))
import Control.Monad (forM_, filterM, void, liftM, when)
import Control.Monad.Error (MonadError(..), runErrorT, ErrorT, Error(..))
import Data.List
import System.Exit

main = do
    result <- Vm.runVirtualMachine "localhost" (PortNumber 2044) body
    putStrLn "Execution ok"

body :: Vm.VirtualMachine IO ()
body = do
    jdv <- Vm.version
    liftIO . putStrLn $ "JdwpVersion: " ++ (show jdv)
    es <- ES.removeEvent
    liftIO . putStrLn $ case ES.suspendPolicy es of
                            SuspendAll -> "this is suspend all"
                            SuspendNone -> "this is suspend none"
                            SuspendEventThread -> "this is suspend event thread"
    liftIO . putStrLn $ show es
    rd <- ER.enable ER.createClassPrepareRequest
    liftIO . putStrLn $ show rd

    pollEvents $ \e -> case E.eventKind e of
        E.ClassPrepare -> isMainClass $ E.referenceType e
        _ -> False

    classes <- Vm.allClasses
    cNames <- mapM name classes
    liftIO . putStrLn $ intercalate "\n" cNames
    threads <- Vm.allThreads
    liftIO . putStrLn $ intercalate "\n" (map show threads)
    filteredClasses <- Vm.classesByName "java.io.BufferedReader"
    liftIO . putStrLn $ intercalate "\n" (map show filteredClasses)
    threadGroups <- Vm.topLevelThreadGroups
    liftIO . putStrLn $ intercalate "\n" (map show threadGroups)
    let mainClass = head $ filter isMainClass classes
    fields <- RT.allFields mainClass
    when (length fields /= 2) $ liftIO exitFailure
    liftIO . putStrLn $ "Main class fields: " ++ show fields
    sName <- sourceName mainClass
    liftIO . putStrLn $ "Main class source name: " ++ sName
    methods <- RT.allMethods mainClass
    liftIO . putStrLn $ "Methods for class " ++ (show mainClass)
    liftIO . putStrLn $ intercalate "\n" (map show methods)
    liftIO . putStrLn =<< Vm.vmName
    liftIO . putStrLn =<< (name $ head methods)
    forM_ threads (\thread -> liftIO . putStrLn =<< name thread)
    let isMainMethod = (liftM ("main" ==)) . name
    methodMain <- head <$> (filterM isMainMethod methods)
    let isAnotherMethod = (liftM ("anotherMethod" ==)) . name
    anotherMethod <- head <$> (filterM isAnotherMethod methods)
    liftIO . putStrLn $ "Variables of method anotherMethod: " ++ (show anotherMethod)
    do
        l <- arguments anotherMethod
        liftIO $ putStrLn $ show l
     `catchError`
            (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))
    liftIO . putStrLn $ "Variables of method main: " ++ (show methodMain)
    do
        l <- variables methodMain
        liftIO $ putStrLn $ show l
     `catchError`
            (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))

    liftIO . putStrLn $ "VariablesByName"
    do
        l <- variablesByName methodMain "i"
        liftIO $ putStrLn $ show l
     `catchError`
            (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))
    liftIO . putStrLn $ "Arguments of the method main"
    do
        mainArgs <- arguments methodMain
        liftIO $ putStrLn $ show mainArgs
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
    bpr <- ER.enable $ ER.createBreakpointRequest mainLocation
    liftIO . putStrLn $ "breakpoint request is enabled"
    ev <- pollEvents $ \e -> case E.eventKind e of
        E.Breakpoint -> True
        _ -> False
    liftIO . putStrLn $ "breakpoint stopped at location"
    liftIO . putStrLn $ show methodMain
    loc <- location ev
    liftIO . putStrLn $ show loc
    liftIO . putStrLn $ "Values of args"

    let curThread = E.thread ev
    fr <- head <$> allFrames curThread
    mainArgs <- arguments methodMain
    mainArgsValue <- stackFrameGetValue fr (head mainArgs)
    case mainArgsValue of
        ArrayValue arrV -> do
            (StringValue aV) <- getArrValue arrV 0
            sV <- stringValue aV
            liftIO $ putStrLn sV
        otherwise  -> liftIO $ putStrLn "Not array value"


    spr <- ER.enable $ (ER.createStepRequest (E.thread ev) StepLine StepOver)
    Vm.resumeVm
    void $ ES.removeEvent
    fieldValues <- mapM (RT.refTypeGetValue mainClass) fields
    checkFieldValues fieldValues
    Vm.resumeVm
    void $ ES.removeEvent

    liftIO . putStrLn $ "trying step requests"
    Vm.resumeVm
    es0 <- ES.removeEvent
    let e0 = head $ ES.events es0
    liftIO $ putStrLn $ show e0

    do
        v0 <- getValueOfI $ E.thread e0
        liftIO $ putStrLn $ show v0
     `catchError`
        (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))

    Vm.resumeVm
    void $ ES.removeEvent
    Vm.resumeVm
    es1 <- ES.removeEvent
    let e1 = head $ ES.events es1
    liftIO $ putStrLn $ show e1

    do
        v1 <- getValueOfI $ E.thread e1
        liftIO $ putStrLn $ show v1
     `catchError`
        (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))

    pollEvents $ \e -> case E.eventKind e of
        E.VmDeath -> True
        _ -> False
    liftIO . putStrLn $ "Exiting"

checkFieldValues fieldValues = do
    liftIO . putStrLn $ "Main class field values: " ++ show fieldValues
    when ((intValue $ fieldValues !! 0) /= 10)
                    $ throwError $ strMsg "field value not equals 10"
    sv <- strValue $ fieldValues !! 1
    when (sv /= "fprivate_value")
                    $ throwError $ strMsg "field value not equals fprivate_value"

intValue (IntValue v) = v

strValue (StringValue sv) = stringValue sv

getValueOfI curThread = do
    frCnt <- frameCount curThread
    liftIO $ putStrLn $ "Frame count: " ++ (show frCnt)
    fr <- head <$> allFrames curThread
    liftIO $ putStrLn $ show fr
    loc <- location fr
    liftIO $ putStrLn $ show loc
    var <- head <$> variablesByName (method loc) "i"
    liftIO $ putStrLn $ show var
    stackFrameGetValue fr var

pollEvents stopFunction = do
    Vm.resumeVm
    es <- ES.removeEvent
    liftIO $ putStrLn $ show es
    let e = head $ ES.events es
    if stopFunction e
        then return e
        else pollEvents stopFunction

isMainClass ref = "LMain;" == RT.genericSignature ref

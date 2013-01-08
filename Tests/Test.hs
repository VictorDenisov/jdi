module Main where

import Language.Java.Jdi
import qualified Language.Java.Jdi.VirtualMachine as Vm
import qualified Language.Java.Jdi.Event as E
import qualified Language.Java.Jdi.EventSet as ES
import qualified Language.Java.Jdi.EventRequest as ER
import qualified Language.Java.Jdi.ReferenceType as RT
import qualified Language.Java.Jdi.ArrayReference as AR
import qualified Language.Java.Jdi.StringReference as SR
import qualified Language.Java.Jdi.Value as V
import qualified Language.Java.Jdi.StackFrame as SF
import qualified Language.Java.Jdi.ThreadReference as TR
import qualified Language.Java.Jdi.ThreadGroupReference as TG
import qualified Language.Java.Jdi.ObjectReference as OR
import qualified Language.Java.Jdi.Method as M
import qualified Language.Java.Jdi.Location as L

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
        E.ClassPrepare -> isMainClass $ referenceType e
        _ -> False

    classes <- Vm.allClasses
    let cNames = map name classes
    liftIO . putStrLn $ intercalate "\n" cNames
    threads <- Vm.allThreads
    liftIO . putStrLn $ intercalate "\n" (map show threads)
    filteredClasses <- Vm.classesByName "java.io.BufferedReader"
    liftIO . putStrLn $ intercalate "\n" (map show filteredClasses)
    threadGroups <- Vm.topLevelThreadGroups
    liftIO . putStrLn $ intercalate "\n" (map show threadGroups)
    let mainClass = head $ filter isMainClass classes

    liftIO . putStrLn $ "Before fields of main class"

    fields <- RT.fields mainClass

    liftIO . putStrLn $ "After fields of main class"
    liftIO . putStrLn $ show fields

    checkFieldsNames fields
    liftIO . putStrLn $ "Main class fields: " ++ show fields
    liftIO . putStrLn $ "Main class fields static: " ++ (show $ map isStatic fields)
    liftIO . putStrLn $ "Main class fields public: " ++ (show $ map isPublic fields)
    liftIO . putStrLn $ "Main class fields private: " ++ (show $ map isPrivate fields)
    sName <- sourceName mainClass
    liftIO . putStrLn $ "Main class source name: " ++ sName
    mainClassInterfaces <- RT.interfaces mainClass

    mainSuper <- RT.superclass mainClass
    liftIO . putStrLn $ "Super classs of main class: " ++ show mainSuper

    let oneInterface = head mainClassInterfaces
    otherInterfaces <- RT.interfaces oneInterface

    liftIO . putStrLn $ "Other interfaces: " ++ show otherInterfaces
    liftIO . putStrLn $ "Main class interfaces: " ++ show mainClassInterfaces
    methods <- RT.methods mainClass
    liftIO . putStrLn $ "Methods for class " ++ (show mainClass)
    liftIO . putStrLn $ intercalate "\n" (map show methods)
    liftIO . putStrLn =<< Vm.name
    liftIO . putStrLn $ name $ head methods
    forM_ threads (\thread -> liftIO . putStrLn $ name thread)
    let isMainMethod = ("main" ==) . name
    let isRunMethod = ("run" ==) . name
    let methodMain = head $ filter isMainMethod methods
    let runMethod = head $ filter isRunMethod methods
    let isAnotherMethod = ("anotherMethod" ==) . name
    let anotherMethod = head $ filter isAnotherMethod methods
    liftIO . putStrLn $ "Variables of method anotherMethod: " ++ (show anotherMethod)
    do
        l <- M.arguments anotherMethod
        liftIO $ putStrLn $ show l
     `catchError`
            (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))
    liftIO . putStrLn $ "Variables of method main: " ++ (show methodMain)
    do
        l <- M.variables methodMain
        liftIO $ putStrLn $ show l
     `catchError`
            (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))

    liftIO . putStrLn $ "VariablesByName"
    do
        l <- M.variablesByName methodMain "i"
        liftIO $ putStrLn $ show l
     `catchError`
            (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))
    liftIO . putStrLn $ "Arguments of the method main"
    do
        mainArgs <- M.arguments methodMain
        liftIO $ putStrLn $ show mainArgs
     `catchError`
            (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))
    liftIO . putStrLn $ "Printing line table"
    lineTable <- allLineLocations methodMain
    liftIO . putStrLn $ "After line table"
    mainLocation <- location methodMain
    runLocation <- location runMethod
    liftIO . putStrLn $ intercalate "\n" (map show lineTable)
    classLineLocations <- allLineLocations mainClass
    liftIO . putStrLn $ intercalate "\n" (map show classLineLocations)
    liftIO . putStrLn $ "Enabling breakpoint request"
    bpr <- ER.enable $ ER.createBreakpointRequest mainLocation
    liftIO . putStrLn $ "breakpoint request is enabled"
    ev <- pollEvents $ \e -> case E.eventKind e of
        E.Breakpoint -> True
        _ -> False
    printThreadTree
    liftIO . putStrLn $ "breakpoint stopped at location"
    liftIO . putStrLn $ show methodMain
    loc <- location ev
    liftIO . putStrLn $ show loc
    liftIO . putStrLn $ "Values of args"

    curThread <- E.thread ev
    fr <- head <$> TR.allFrames curThread
    mainArgs <- M.arguments methodMain
    mainArgsValue <- SF.getValue fr (head mainArgs)
    case mainArgsValue of
        V.ArrayValue arrV -> do
            (V.StringValue aV) <- AR.getValue arrV 0
            sV <- SR.stringValue aV
            liftIO $ putStrLn sV
        otherwise  -> liftIO $ putStrLn "Not array value"

    bpr <- ER.enable $ ER.createBreakpointRequest runLocation
    ev <- pollEvents $ \e -> case E.eventKind e of
        E.Breakpoint -> True
        _ -> False

    evThread <- E.thread ev
    spr <- ER.enable $ (ER.createStepRequest evThread StepLine StepOver)
    Vm.resume
    ev1 <- ES.removeEvent
    fieldValues <- mapM (RT.getValue mainClass) (take 2 fields)
    checkFieldValues fieldValues

    liftIO . putStrLn $ "==== taking value of non static field in reftype ===="
    (void $ RT.getValue mainClass (last fields))
        `catchError`
            (\ee -> liftIO $ putStrLn $ "error message: " ++ (show ee))
    liftIO . putStrLn $ "===== ============ ======"

    liftIO . putStrLn $ "===== this Object values ======"
    let evv = head $ ES.events ev1
    curThread <- E.thread evv
    fr1 <- head <$> TR.allFrames curThread
    thisObj <- SF.thisObject fr1
    argsValue <- OR.getValue thisObj (last fields)
    liftIO . putStrLn $ show argsValue
    liftIO . putStrLn $ "===== =========== ======"

    Vm.resume
    void $ ES.removeEvent

    liftIO . putStrLn $ "trying step requests"
    Vm.resume
    es0 <- ES.removeEvent
    let e0 = head $ ES.events es0
    liftIO $ putStrLn $ show e0

    do
        e0thread <- E.thread e0
        v0 <- getValueOfI e0thread
        liftIO $ putStrLn $ show v0
     `catchError`
        (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))

    Vm.resume
    void $ ES.removeEvent
    Vm.resume
    es1 <- ES.removeEvent
    let e1 = head $ ES.events es1
    liftIO $ putStrLn $ show e1

    do
        e1thread <- E.thread e1
        v1 <- getValueOfI e1thread
        liftIO $ putStrLn $ show v1
     `catchError`
        (\ee -> liftIO $ putStrLn $ "error during arguments: " ++ (show ee))

    pollEvents $ \e -> case E.eventKind e of
        E.VmDeath -> True
        _ -> False
    liftIO . putStrLn $ "Exiting"

printThreadTree = do
    liftIO $ putStrLn "========== Thread Tree ==========="
    tgs <- Vm.topLevelThreadGroups
    mapM (printThreadGroup 0) tgs
    liftIO $ putStrLn "========== ----------- ==========="

printThreadGroup depth tg = do
    let is = "   "
    let indent = concat $ replicate depth is
    let tgName = name tg
    liftIO $ putStrLn $ indent ++ "Thread group: " ++ tgName

    liftIO $ putStrLn $ indent ++ is ++ "Threads: "
    trds <- TG.threads tg
    tstats <- mapM ((show <$>) . TR.status) trds
    tsusps <- mapM ((show <$>) . TR.isSuspended) trds
    let ts = map name trds
    let threadStrings = zipWith3 (\a b c -> a ++ " " ++ b ++ " " ++ c) ts tstats tsusps
    liftIO $ putStrLn $ intercalate "\n" $ map ((indent ++ is ++ is) ++ ) threadStrings

    mapM (printThreadGroup $ depth + 1) =<< TG.threadGroups tg
    return ()

checkFieldsNames fields = do
    when (length fields /= 3) $ liftIO exitFailure
    let f1name = name (fields !! 0)
    let f2name = name (fields !! 1)
    let f3name = name (fields !! 2)
    when (f1name /= "f1") $ liftIO exitFailure
    when (f2name /= "fprivate") $ liftIO exitFailure
    when (f3name /= "args") $ liftIO exitFailure

checkFieldValues fieldValues = do
    liftIO . putStrLn $ "Main class field values: " ++ show fieldValues
    when ((intValue $ fieldValues !! 0) /= 10)
                    $ throwError $ strMsg "field value not equals 10"
    sv <- strValue $ fieldValues !! 1
    when (sv /= "fprivate_value")
                    $ throwError $ strMsg "field value not equals fprivate_value"

intValue (V.IntValue v) = v

strValue (V.StringValue sv) = SR.stringValue sv

getValueOfI curThread = do
    frCnt <- TR.frameCount curThread
    liftIO $ putStrLn $ "Frame count: " ++ (show frCnt)
    fr <- head <$> TR.allFrames curThread
    liftIO $ putStrLn $ show fr
    loc <- location fr
    liftIO $ putStrLn $ show loc
    var <- head <$> M.variablesByName (L.method loc) "i"
    liftIO $ putStrLn $ show var
    SF.getValue fr var

pollEvents stopFunction = do
    Vm.resume
    es <- ES.removeEvent
    liftIO $ putStrLn $ show es
    let e = head $ ES.events es
    if stopFunction e
        then return e
        else pollEvents stopFunction

isMainClass ref = "LMain;" == signature ref

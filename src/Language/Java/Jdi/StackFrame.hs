module Language.Java.Jdi.StackFrame
( StackFrame
, getValue
, thisObject
, location
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import Control.Monad (when)
import Data.Binary.Get (runGet)
import qualified Language.Java.Jdi.Method as M
import qualified Language.Java.Jdwp as J

{- | Gets the 'Value' of a 'LocalVariable' in this frame. The variable must be valid
for this frame's method and visible according to the rules described in
visibleVariables().
-}
getValue :: (Error e, MonadIO m, MonadError e m) =>
            StackFrame -> LocalVariable -> VirtualMachine m Value
getValue (StackFrame (ThreadReference _ ti) (J.StackFrame fi _))
         (LocalVariable _ _ slot) = do
    reply <- runCommand $ J.getValuesCommand ti fi [slot]
    idsizes <- getIdSizes
    return $ toJdiValue $ head $ runGet
                        (J.parseGetValuesReply idsizes)
                        (J.toLazy $ J.dat reply)


{- | Returns the value of this for the current frame. The ObjectReference for
'this' is only available for non-native instance methods.
-}
thisObject :: (Error e, MonadIO m, MonadError e m)
           => StackFrame
           -> VirtualMachine m J.ObjectReference {- ^ an ObjectReference,
           or null ObjectReference if the frame represents a native
           or static method. -}
thisObject sf@(StackFrame (ThreadReference _ ti) (J.StackFrame fi _)) = do
    loc <- location sf
    let mtd = method loc
    when (M.isStatic mtd) $
                throwError $ strMsg "Can get this object for static method"
    reply <- runCommand $ J.thisObjectCommand ti fi
    idsizes <- getIdSizes
    let (ObjectValue ref) = toJdiValue $ runGet
                        (J.parseTaggedValue idsizes)
                        (J.toLazy $ J.dat reply)
    return ref

{- |
Returns the 'Location' of the current instruction in the frame. The method for
which this frame was created can also be accessed through the returned
location. For the top frame in the stack, this location identifies the next
instruction to be executed. For all other frames, this location identifies the
instruction that caused the next frame's method to be invoked. If the frame
represents a native method invocation, the returned location indicates the
class and method, but the code index will not be valid (-1).
 -}
location :: (Error e, MonadIO m, MonadError e m)
         => StackFrame -> VirtualMachine m Location
location (StackFrame _ (J.StackFrame _ javaLoc))
                    = locationFromJavaLocation javaLoc

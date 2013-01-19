module Language.Java.Jdi.StackFrame
( StackFrame
, getValue
, thisObject
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import Control.Monad (when)
import Data.Binary.Get (runGet)
import qualified Language.Java.Jdi.Method as M
import qualified Language.Java.Jdwp as J

{- | Gets the Value of a LocalVariable in this frame. The variable must be valid
for this frame's method and visible according to the rules described in
visibleVariables().
-}
getValue :: (Error e, MonadIO m, MonadError e m) =>
            StackFrame -> LocalVariable -> VirtualMachine m Value
getValue = stackFrameGetValue

{- | Returns the value of 'this' for the current frame. The ObjectReference for
'this' is only available for non-native instance methods.

Returns: an ObjectReference, or null ObjectReference if the frame represents a
native or static method.
-}
thisObject :: (Error e, MonadIO m, MonadError e m)
           => StackFrame -> VirtualMachine m J.ObjectReference
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

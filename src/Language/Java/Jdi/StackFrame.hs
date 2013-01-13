module Language.Java.Jdi.StackFrame
( StackFrame
, getValue
, thisObject
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

{- | Gets the Value of a LocalVariable in this frame. The variable must be valid
for this frame's method and visible according to the rules described in
visibleVariables().
-}
getValue :: (Error e, MonadIO m, MonadError e m) =>
            StackFrame -> LocalVariable -> VirtualMachine m Value
getValue = stackFrameGetValue

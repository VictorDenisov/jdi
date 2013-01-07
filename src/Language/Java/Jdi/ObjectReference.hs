module Language.Java.Jdi.ObjectReference
( ObjectReference
, disableCollection
, enableCollection
, entryCount
, getValue
, getValues
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

getValue :: (Error e, MonadIO m, MonadError e m)
            => ObjectReference -> Field -> VirtualMachine m Value
getValue = objGetValue

getValues :: (Error e, MonadIO m, MonadError e m)
            => ObjectReference -> [Field] -> VirtualMachine m [Value]
getValues = objGetValues

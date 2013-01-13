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

{- | Gets the value of a given instance or static field in this object.
The Field must be valid for this ObjectReference; that is, it must be from the
mirrored object's class or a superclass of that class.
-}
getValue :: (Error e, MonadIO m, MonadError e m)
            => ObjectReference -> Field -> VirtualMachine m Value
getValue = objGetValue

{- | Gets the value of multiple instance and/or static fields in this object.
The Fields must be valid for this ObjectReference; that is, they must be from
the mirrored object's class or a superclass of that class. -}
getValues :: (Error e, MonadIO m, MonadError e m)
            => ObjectReference -> [Field] -> VirtualMachine m [Value]
getValues = objGetValues

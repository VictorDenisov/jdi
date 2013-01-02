module Language.Java.Jdi.ArrayReference
( ArrayReference
, getValue
, getValues
, length
) where

import Prelude hiding (length)
import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

getValue :: (Error e, MonadIO m, MonadError e m) =>
               ArrayReference -> Int -> VirtualMachine m Value
getValue = getArrValue

getValues :: (Error e, MonadIO m, MonadError e m) =>
                ArrayReference -> VirtualMachine m [Value]
getValues = getArrValues

length :: (Error e, MonadIO m, MonadError e m) =>
             ArrayReference -> VirtualMachine m Int
length = arrLength

module Language.Java.Jdi.ArrayReference
( J.ArrayReference
, getValue
, getValues
, length
) where

import Prelude hiding (length)
import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import qualified Language.Java.Jdwp as J

{- | Returns an array component value. -}
getValue :: (Error e, MonadIO m, MonadError e m) =>
               J.ArrayReference -> Int -> VirtualMachine m Value
getValue = getArrValue

{- | Returns all of the components in this array. -}
getValues :: (Error e, MonadIO m, MonadError e m) =>
                J.ArrayReference -> VirtualMachine m [Value]
getValues = getArrValues

{- | Returns the number of components in this array. -}
length :: (Error e, MonadIO m, MonadError e m) =>
             J.ArrayReference -> VirtualMachine m Int
length = arrLength

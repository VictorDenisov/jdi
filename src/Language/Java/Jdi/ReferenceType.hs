module Language.Java.Jdi.ReferenceType
( ReferenceType
, getValue
, fields
, methods
, interfaces
, superclass
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

getValue :: (Error e, MonadIO m, MonadError e m) =>
                   ReferenceType -> Field -> VirtualMachine m Value
getValue = refTypeGetValue

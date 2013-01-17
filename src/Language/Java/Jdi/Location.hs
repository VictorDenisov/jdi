module Language.Java.Jdi.Location
( Location
, codeIndex
, declaringType
, lineNumber
, method
, sourceName
) where

import Language.Java.Jdi.Impl hiding (declaringType)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

declaringType :: Location -> ReferenceType
declaringType = locationDeclaringType

sourceName :: (Error e , MonadIO m, MonadError e m)
           => Location -> VirtualMachine m String
sourceName = locationSourceName

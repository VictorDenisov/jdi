module Language.Java.Jdi.Location
( Location
, codeIndex
, declaringType
, lineNumber
, method
, sourceName
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import qualified Language.Java.Jdwp as J

declaringType :: Location -> J.ReferenceType
declaringType = locationDeclaringType

sourceName :: (Error e , MonadIO m, MonadError e m)
           => Location -> VirtualMachine m String
sourceName = locationSourceName

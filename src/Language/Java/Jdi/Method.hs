module Language.Java.Jdi.Method
( Method
, arguments
, variables
, variablesByName
, allLineLocations
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

allLineLocations :: (Error e, MonadIO m, MonadError e m)
                 => Method -> VirtualMachine m [Location]
allLineLocations = methodAllLineLocations

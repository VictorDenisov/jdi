module Language.Java.Jdi.ReferenceType
( ReferenceType
, getValue
, fields
, methods
, interfaces
, superclass
, name
, signature
, allLineLocations
) where

import Language.Java.Jdi.Impl hiding (name, signature)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

{- | Gets the Value of a given static Field in this type. The Field must be valid
for this type; that is, it must be declared in this type, a superclass, a
superinterface, or an implemented interface.
-}
getValue :: (Error e, MonadIO m, MonadError e m) =>
                   ReferenceType -> Field -> VirtualMachine m Value
getValue = refTypeGetValue

name :: ReferenceType -> String
name = refTypeName

signature :: ReferenceType -> String
signature = refTypeSignature

allLineLocations :: (Error e, MonadIO m, MonadError e m)
                 => ReferenceType -> VirtualMachine m [Location]
allLineLocations = refTypeAllLineLocations

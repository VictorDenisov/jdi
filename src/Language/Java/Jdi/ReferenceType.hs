module Language.Java.Jdi.ReferenceType
( J.ReferenceType
, getValue
, fields
, methods
, interfaces
, superclass
, name
, signature
, allLineLocations
, sourceName
) where

import Language.Java.Jdi.Impl
import qualified Language.Java.Jdwp as J
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import qualified Language.Java.Jdi.Field as F
import Control.Monad (when)
import Data.Binary.Get (runGet)

name :: J.ReferenceType -> String
name = refTypeName

signature :: J.ReferenceType -> String
signature = refTypeSignature

allLineLocations :: (Error e, MonadIO m, MonadError e m)
                 => J.ReferenceType -> VirtualMachine m [Location]
allLineLocations = refTypeAllLineLocations

sourceName :: (Error e , MonadIO m, MonadError e m)
           => J.ReferenceType -> VirtualMachine m String
sourceName = refTypeSourceName

{- | Gets the Value of a given static Field in this type. The Field must be
valid for this type; that is, it must be declared in this type, a superclass,
a superinterface, or an implemented interface.

Program should be started to be sure static fields are properly
initialized.
 -}
getValue :: (Error e, MonadIO m, MonadError e m) =>
                   J.ReferenceType -> Field -> VirtualMachine m Value
getValue (J.ReferenceType _ ri _ _) field@(Field _ f) = do
    when (not $ F.isStatic field)
                $ throwError $ strMsg "Only static fields are allowed in ReferenceType getValue"
    reply <- runCommand $ J.refGetValuesCommand ri [f]
    idsizes <- getIdSizes
    return $ toJdiValue $ head $ runGet
                        (J.parseGetValuesReply idsizes)
                        (J.toLazy $ J.dat reply)

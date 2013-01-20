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
import Control.Monad (liftM)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import qualified Language.Java.Jdi.Field as F
import Control.Monad (when)
import Data.Binary.Get (runGet)

{- | Gets the fully qualified name of this type. The returned name is formatted
as it might appear in a Java programming langauge declaration for objects of
this type.

For primitive classes the returned name is the name of the corresponding
primitive type; for example, int is returned as the name of the class
represented by Integer.TYPE.
-}
name :: J.ReferenceType -> String
name = signatureToName . signature

{- | Returns the JNI-style signature for this type.

For primitive classes the returned signature is the signature of the
corresponding primitive type; for example, I is returned as the signature of
the class represented by Integer.TYPE.
-}
signature :: J.ReferenceType -> String
signature (J.ReferenceType _ _ gs _) = gs

{- | Returns a list containing a 'Location' object for each executable source
line in this reference type.

This method is equivalent to allLineLocations(vm.getDefaultStratum(),null) -
see allLineLocations :: String -> String for more information
(/it doesn't exist yet/).
-}
allLineLocations :: (Error e, MonadIO m, MonadError e m)
                 => J.ReferenceType -> VirtualMachine m [Location]
allLineLocations refType =
    concat `liftM` ((mapM methodAllLineLocations) =<< (methods refType))

{- | Gets an identifying name for the source corresponding to the declaration of
this type. Interpretation of this string is the responsibility of the source
repository mechanism.

The returned name is dependent on VM's default stratum
(VirtualMachine.getDefaultStratum()). In the reference implementation, when
using the base stratum, the returned string is the unqualified name of the
source file containing the declaration of this type. In other strata the
returned source name is the first source name for that stratum. Since other
languages may have more than one source name for a reference type, the use of
Location.sourceName() or sourceNames(String) is preferred.

For arrays (ArrayType) and primitive classes, error is occured.
-}
sourceName :: (Error e , MonadIO m, MonadError e m)
           => J.ReferenceType -> VirtualMachine m String
sourceName (J.ReferenceType _ refId _ _) = sourceNameFromRefId refId

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

{- | Returns a list containing each 'Field' declared in this type. Inherited
fields are not included. Any synthetic fields created by the compiler are
included in the list.

For arrays ('ArrayType') and primitive classes, the returned list is always
empty.
-}
fields :: (Error e, MonadIO m, MonadError e m) =>
             J.ReferenceType -> VirtualMachine m [Field]
fields rt@(J.ReferenceType _ refId _ _) = do
    idsizes <- getIdSizes
    reply <- runCommand $ J.fieldsCommand refId
    let r = J.dat reply
    let fields = runGet (J.parseFieldsReply idsizes) (J.toLazy r)
    return $ map (Field rt) fields

-- | For interfaces returns interfaces extended by this interface.
interfaces :: (Error e, MonadIO m, MonadError e m) =>
              J.ReferenceType -> VirtualMachine m [J.ReferenceType]
interfaces (J.ReferenceType _ refId _ _) = do
    reply <- runCommand $ J.interfacesCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let interfaceIds = runGet (J.parseInterfacesReply idsizes) (J.toLazy r)
    mapM (referenceTypeFromRefId J.Interface) interfaceIds

-- | Doesn't work for 'ReferenceTypes' that are interfaces.
superclass (J.ReferenceType _ refId _ _) = do
    reply <- runCommand $ J.superclassCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let subRefId = runGet
                (J.parseReferenceTypeId $ J.referenceTypeIdSize idsizes)
                (J.toLazy r)
    referenceTypeFromRefId J.Class subRefId

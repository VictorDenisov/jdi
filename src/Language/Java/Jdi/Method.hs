module Language.Java.Jdi.Method
( Method
, arguments
, variables
, variablesByName
, allLineLocations
, isPackagePrivate
, isPrivate
, isProtected
, isPublic
, modifiers
, name
, declaringType
, signature
, isFinal
, isStatic
, isSynthetic
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import Data.Bits ((.&.))
import qualified Language.Java.Jdwp as J

allLineLocations :: (Error e, MonadIO m, MonadError e m)
                 => Method -> VirtualMachine m [Location]
allLineLocations = methodAllLineLocations

isPackagePrivate f = not (isPrivate f)
                  && not (isProtected f)
                  && not (isPublic f)
isPrivate (Method _ (J.Method _ _ _ modbits))
                            = (modbits .&. J.method_private) /= 0
isProtected (Method _ (J.Method _ _ _ modbits))
                            = (modbits .&. J.method_protected) /= 0
isPublic (Method _ (J.Method _ _ _ modbits))
                            = (modbits .&. J.method_public) /= 0
modifiers (Method _ (J.Method _ _ _ modbits)) = fromIntegral modbits

name (Method _ (J.Method _ name _ _)) = name

declaringType (Method rt _) = rt

signature (Method _ (J.Method _ _ sig _)) = sig

isFinal (Method _ (J.Method _ _ _ modbits))
                        = (J.method_final .&. modbits) /= 0
isStatic (Method _ (J.Method _ _ _ modbits))
                        = (J.method_static .&. modbits) /= 0
isSynthetic (Method _ (J.Method _ _ _ modbits))
                        = (J.method_synthetic .&. modbits) /= 0

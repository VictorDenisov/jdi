module Language.Java.Jdi.Field
( Field
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
import qualified Language.Java.Jdwp as J
import Data.Bits ((.&.))

isPackagePrivate f = not (isPrivate f)
                       && not (isProtected f)
                       && not (isPublic f)

isPrivate (Field _ (J.Field _ _ _ modbits))
                            = (modbits .&. J.field_private) /= 0
isProtected (Field _ (J.Field _ _ _ modbits))
                            = (modbits .&. J.field_protected) /= 0
isPublic (Field _ (J.Field _ _ _ modbits))
                            = (modbits .&. J.field_public) /= 0
modifiers (Field _ (J.Field _ _ _ modbits))
                            = fromIntegral modbits

name (Field _ (J.Field _ nm _ _)) = nm

declaringType (Field rt _) = rt

signature (Field _ (J.Field _ _ sig _)) = sig

isFinal (Field _ (J.Field _ _ _ modbits))
                        = (J.field_final .&. modbits) /= 0
isStatic (Field _ (J.Field _ _ _ modbits))
                        = (J.field_static .&. modbits) /= 0
isSynthetic (Field _ (J.Field _ _ _ modbits))
                        = (J.field_synthetic .&. modbits) /= 0

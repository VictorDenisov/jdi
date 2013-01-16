module Language.Java.Jdi.Location
( Location
, codeIndex
, declaringType
, lineNumber
, method
) where

import Language.Java.Jdi.Impl hiding (declaringType)

declaringType :: Location -> ReferenceType
declaringType = locationDeclaringType

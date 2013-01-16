module Language.Java.Jdi.LocalVariable
( LocalVariable
, name
) where

import Language.Java.Jdi.Impl hiding (name)

name :: LocalVariable -> String
name = localVariableName

module Language.Java.Jdi.LocalVariable
( LocalVariable
, name
) where

import Language.Java.Jdi.Impl

name :: LocalVariable -> String
name = localVariableName

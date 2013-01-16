module Language.Java.Jdi.ThreadGroupReference
( ThreadGroupReference
, parent
, threadGroups
, threads
, name
) where

import Language.Java.Jdi.Impl hiding (name)

name :: ThreadGroupReference -> String
name = threadGroupRefName

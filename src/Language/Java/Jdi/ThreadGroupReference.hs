module Language.Java.Jdi.ThreadGroupReference
( ThreadGroupReference
, parent
, threadGroups
, threads
, name
) where

import Language.Java.Jdi.Impl

name :: ThreadGroupReference -> String
name = threadGroupRefName

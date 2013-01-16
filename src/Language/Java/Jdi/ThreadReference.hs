module Language.Java.Jdi.ThreadReference
( ThreadReference
, allFrames
, frameCount
, frames
, threadGroup
, status
, isSuspended
, name
) where

import Language.Java.Jdi.Impl hiding (name)

name :: ThreadReference -> String
name = threadRefName

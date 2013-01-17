module Language.Java.Jdi.ThreadReference
( ThreadReference
, allFrames
, frameCount
, frames
, threadGroup
, status
, isSuspended
, name
, resume
) where

import Language.Java.Jdi.Impl hiding (name)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

name :: ThreadReference -> String
name = threadRefName

resume :: (Error e, MonadIO m, MonadError e m)
       => ThreadReference -> VirtualMachine m ()
resume = resumeThreadRef

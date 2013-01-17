module Language.Java.Jdi.EventSet
( EventSet(..)
, removeEvent
, resume
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

resume :: (Error e, MonadIO m, MonadError e m)
       => EventSet -> VirtualMachine m ()
resume = resumeEventSet

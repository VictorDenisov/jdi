module Language.Java.Jdi.EventSet
( J.EventSet(..)
, removeEvent
, resume
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import qualified Language.Java.Jdwp as J

resume :: (Error e, MonadIO m, MonadError e m)
       => J.EventSet -> VirtualMachine m ()
resume = resumeEventSet

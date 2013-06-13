module Language.Java.Jdi.ThreadReference
( ThreadReference
, allFrames
, frameCount
, frames
, frame
, threadGroup
, status
, isSuspended
, name
, resume
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

-- | Returns the name of this thread.
name :: ThreadReference -> String
name (ThreadReference n _) = n

{- | Resumes this thread. If this thread was not previously suspended through
'suspend' or through 'Language.Java.Jdi.VirtualMachine.suspend', or because of
a SUSPEND_ALL or SUSPEND_EVENT_THREAD event, then invoking this method has no
effect.  Otherwise, the count of pending suspends on this thread is decremented.
If it is decremented to 0, the thread will continue to execute.

/Note/: the normal way to resume from an event related suspension is via
'Language.Java.Jdi.EventSet.resume'.
-}
resume :: (Error e, MonadIO m, MonadError e m)
       => ThreadReference -> VirtualMachine m ()
resume (ThreadReference _ tId) = resumeThreadId tId

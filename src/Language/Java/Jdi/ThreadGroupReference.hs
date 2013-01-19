module Language.Java.Jdi.ThreadGroupReference
( ThreadGroupReference
, parent
, threadGroups
, threads
, name
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import Data.Binary.Get (runGet)
import qualified Language.Java.Jdwp as J

-- | Returns the parent of this thread group.
parent :: (Error e, MonadIO m, MonadError e m)
       => ThreadGroupReference
       -> VirtualMachine m ThreadGroupReference {- ^ a 'ThreadGroupReference'
       mirroring the parent of this thread group in the target VM, or null
       if this is a top-level thread group.-}

parent (ThreadGroupReference _ refId) = do
    reply <- runCommand $ J.threadGroupReferenceParentCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let parentId = runGet
                    (J.parseThreadGroupId $ J.threadGroupIdSize idsizes)
                    (J.toLazy r)
    threadGroupReferenceFromId parentId

{- | Returns a list containing each active 'ThreadGroupReference' in this thread
group. Only the active thread groups in this immediate thread group (and not its
subgroups) are returned. See java.lang.ThreadGroup for information about
active ThreadGroups.
-}
threadGroups :: (Error e, MonadIO m, MonadError e m)
             => ThreadGroupReference
             -> VirtualMachine m [ThreadGroupReference] {- ^
                a list of ThreadGroupReference objects mirroring the active
                thread groups from this thread group in the target VM. -}
threadGroups (ThreadGroupReference _ refId) = do
    reply <- runCommand $ J.threadGroupReferenceChildrenCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let (_, groups) = runGet
                        (J.parseThreadGroupChildrenReply idsizes)
                        (J.toLazy r)
    mapM threadGroupReferenceFromId groups

{- | Returns a list containing a 'ThreadReference' for each live thread in this
thread group. Only the live threads in this immediate thread group (and not its
subgroups) are returned. A thread is alive if it has been started and has not
yet been stopped.
-}
threads :: (Error e, MonadIO m, MonadError e m)
             => ThreadGroupReference
             -> VirtualMachine m [ThreadReference] {- ^ a list of
             'ThreadReference' objects mirroring the live threads from this
             thread group in the target VM. -}
threads (ThreadGroupReference _ refId) = do
    reply <- runCommand $ J.threadGroupReferenceChildrenCommand refId
    let r = J.dat reply
    idsizes <- getIdSizes
    let (ts, _) = runGet
                        (J.parseThreadGroupChildrenReply idsizes)
                        (J.toLazy r)
    mapM threadReferenceFromId ts

-- | Returns the name of this thread group.
name :: ThreadGroupReference -> String
name (ThreadGroupReference n _) = n

module Language.Java.Jdi.VirtualMachine
( VirtualMachine
, runVirtualMachine
, name
, description
, version
, allClasses
, allThreads
, canAddMethod
, canBeModified
, canGetBytecodes
, canGetCurrentContendedMonitor
, canGetMonitorInfo
, canGetOwnedMonitorInfo
, canGetSourceDebugExtension
, canGetSynteticAttribute
, canPopFrames
, canRedefineClasses
, canRequestVmDeathEvent
, canUnrestrictedlyRedefineClasses
, canUseInstanceFilters
, canWatchFieldAccess
, canWatchFieldModification
, classesByName
, dispose
, exit
, resume
, suspend
, topLevelThreadGroups
) where

import Language.Java.Jdi.Impl hiding (name, resume)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

-- | Returns the name of the target VM as reported by the property java.vm.name.
name :: MonadIO m => VirtualMachine m String
name = vmName

{- | Continues the execution of the application running in this virtual machine.
All threads are resumed as documented in ThreadReference.resume().
-}
resume :: (Error e, MonadIO m, MonadError e m) => VirtualMachine m ()
resume = resumeVm

{- | Suspends the execution of the application running in this virtual machine.
All threads currently running will be suspended.

Unlike Thread.suspend(), suspends of both the virtual machine and individual
threads are counted. Before a thread will run again, it must be resumed (through
resume() or ThreadReference.resume()) the same number of times it has been
suspended. -}
suspend :: (Error e, MonadIO m, MonadError e m) => VirtualMachine m ()
suspend = suspendVm

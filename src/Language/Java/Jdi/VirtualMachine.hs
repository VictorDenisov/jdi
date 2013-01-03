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

name :: MonadIO m => VirtualMachine m String
name = vmName

resume :: (Error e, MonadIO m, MonadError e m) => VirtualMachine m ()
resume = resumeVm

suspend :: (Error e, MonadIO m, MonadError e m) => VirtualMachine m ()
suspend = suspendVm

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
, topLevelThreadGroups
) where

import Language.Java.Jdi.Impl hiding (name, resume)
import Control.Monad.IO.Class (MonadIO)

name :: MonadIO m => VirtualMachine m String
name = vmName

resume :: MonadIO m => VirtualMachine m ()
resume = resumeVm

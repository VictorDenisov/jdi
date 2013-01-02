module Language.Java.Jdi
( VirtualMachine
, runVirtualMachine
, Name(..)
, Resumable(..)
, Locatable(..)
, SourceName(..)
, AllLineLocations(..)
, vmName
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
, resumeVm
, topLevelThreadGroups
, Event
, referenceType
, thread
, EventKind(..)
, eventKind
, EventSet(..)
, removeEvent
, EventRequest
, enable
, disable
, addCountFilter
, createClassPrepareRequest
, createBreakpointRequest
, createStepRequest
, ReferenceType
, genericSignature
, refTypeGetValue
, allFields
, allMethods
, ArrayReference
, getArrValue
, getArrValues
, arrLength
, StringReference
, stringValue
, Value(..)
, StackFrame
, stackFrameGetValue
, ThreadReference
, allFrames
, frameCount
, frames
, ThreadGroupReference
, StepSize(..)
, StepDepth(..)
, Field
, Method
, arguments
, variables
, variablesByName
, LocalVariable
, Location
, codeIndex
, declaringType
, lineNumber
, method
, SuspendPolicy(..)
) where

import Language.Java.Jdi.Impl


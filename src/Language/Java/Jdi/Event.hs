module Language.Java.Jdi.Event
( J.Event
, thread
, J.EventKind(..)
, J.eventKind
, referenceType
, location
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))

import qualified Language.Java.Jdwp as J

location :: (Error e, MonadIO m, MonadError e m)
         => J.Event -> VirtualMachine m Location
location (J.BreakpointEvent _ _ javaLocation) =
    locationFromJavaLocation javaLocation
location (J.StepEvent _ _ javaLocation) =
    locationFromJavaLocation javaLocation

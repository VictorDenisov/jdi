module Language.Java.Jdi.Method
( Method
, arguments
, variables
, variablesByName
, allLineLocations
, isPackagePrivate
, isPrivate
, isProtected
, isPublic
, modifiers
, name
, declaringType
, signature
, isFinal
, isStatic
, isSynthetic
, location
) where

import Language.Java.Jdi.Impl
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import Data.Bits ((.&.))
import qualified Language.Java.Jdwp as J

allLineLocations :: (Error e, MonadIO m, MonadError e m)
                 => Method -> VirtualMachine m [Location]
allLineLocations = methodAllLineLocations

isPackagePrivate f = not (isPrivate f)
                  && not (isProtected f)
                  && not (isPublic f)
isPrivate (Method _ (J.Method _ _ _ modbits))
                            = (modbits .&. J.method_private) /= 0
isProtected (Method _ (J.Method _ _ _ modbits))
                            = (modbits .&. J.method_protected) /= 0
isPublic (Method _ (J.Method _ _ _ modbits))
                            = (modbits .&. J.method_public) /= 0
modifiers (Method _ (J.Method _ _ _ modbits)) = fromIntegral modbits

name (Method _ (J.Method _ name _ _)) = name

declaringType (Method rt _) = rt

signature (Method _ (J.Method _ _ sig _)) = sig

isFinal (Method _ (J.Method _ _ _ modbits))
                        = (J.method_final .&. modbits) /= 0
isStatic (Method _ (J.Method _ _ _ modbits))
                        = (J.method_static .&. modbits) /= 0
isSynthetic (Method _ (J.Method _ _ _ modbits))
                        = (J.method_synthetic .&. modbits) /= 0

location :: (Error e, MonadIO m, MonadError e m)
         => Method -> VirtualMachine m Location
location m@(Method ref method) = do
    (J.LineTable _ _ lines) <- receiveLineTable m
    return $ Location ref method (head lines)

{- | Returns a list containing each LocalVariable that is declared as an
argument of this method. If local variable information is not available, values
of actual arguments to method invocations can be obtained by using the function
getArgumentValues of StackFrame.

Returns: the list of LocalVariable arguments. If there are no arguments,
a zero-length list is returned.
-}
arguments :: (Error e, MonadIO m, MonadError e m) =>
             Method -> VirtualMachine m [LocalVariable]
arguments method = getVariables method (>)

{- | Returns a list containing each LocalVariable declared in this method.
The list includes any variable declared in any scope within the method. It may
contain multiple variables of the same name declared within disjoint scopes.
Arguments are considered local variables and will be present in the returned
list. If local variable information is not available, values of actual arguments
to method invocations can be obtained by using the function getArgumentValues
of StackFrame.

Returns: the list of LocalVariable objects which mirror local variables declared in
this method in the target VM. If there are no local variables, a zero-length list is returned.
-}
variables :: (Error e, MonadIO m, MonadError e m) =>
             Method -> VirtualMachine m [LocalVariable]
variables method = getVariables method (<=)

{- | Returns a list containing each LocalVariable of a given name in this
method. Multiple variables can be returned if the same variable name is used in
disjoint scopes within the method.

Returns: the list of LocalVariable objects of the given name. If there are no
matching local variables, a zero-length list is returned.
-}
variablesByName :: (Error e, MonadIO m, MonadError e m) =>
                   Method -> String -> VirtualMachine m [LocalVariable]
variablesByName method varName =
    (filter ((varName ==) . localVariableName)) `liftM` (variables method)

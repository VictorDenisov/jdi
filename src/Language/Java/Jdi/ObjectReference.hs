module Language.Java.Jdi.ObjectReference
( J.ObjectReference
, disableCollection
, enableCollection
, entryCount
, getValue
, getValues
) where

import Language.Java.Jdi.Impl
import Data.Binary.Get (runGet)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import qualified Language.Java.Jdwp as J

{- | Gets the value of a given instance or static field in this object.
The Field must be valid for this ObjectReference; that is, it must be from the
mirrored object's class or a superclass of that class.
-}
getValue :: (Error e, MonadIO m, MonadError e m)
            => J.ObjectReference -> Field -> VirtualMachine m Value
getValue (J.ObjectReference ri) (Field _ f) = do
    reply <- runCommand $ J.objGetValuesCommand ri [f]
    idsizes <- getIdSizes
    return $ toJdiValue $ head $ runGet
                        (J.parseGetValuesReply idsizes)
                        (J.toLazy $ J.dat reply)

{- | Gets the value of multiple instance and/or static fields in this object.
The Fields must be valid for this ObjectReference; that is, they must be from
the mirrored object's class or a superclass of that class. -}
getValues :: (Error e, MonadIO m, MonadError e m)
            => J.ObjectReference -> [Field] -> VirtualMachine m [Value]
getValues (J.ObjectReference ri) fs = do
    reply <- runCommand $ J.objGetValuesCommand ri (map getFid fs)
    idsizes <- getIdSizes
    return $ map toJdiValue $ runGet (J.parseGetValuesReply idsizes)
                             (J.toLazy $ J.dat reply)
    where getFid (Field _ f) = f


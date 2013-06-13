module Language.Java.Jdi.StringReference
( J.StringReference
, value
) where

import Language.Java.Jdi.Impl
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import Data.Binary.Get (runGet)
import qualified Language.Java.Jdwp as J

{- | Returns the 'J.StringReference' as a String. The returned string is the
equivalent of the mirrored string, it can be manipulated like any other string.
-}
value :: (Error e, MonadIO m, MonadError e m)
      => J.StringReference -> VirtualMachine m String
value sr@(J.StringReference sid) = do
    reply <- runCommand $ J.stringValueCommand sid
    return $ runGet J.parseString (J.toLazy $ J.dat reply)

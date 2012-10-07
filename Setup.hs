import Distribution.Simple
import Distribution.PackageDescription
import Data.Monoid (Monoid(mempty, mappend))

main = defaultMainWithHooks (simpleUserHooks { preInst = myPreInst })

myPreInst a f = return (Nothing, [("Test", mempty { buildable = False})])

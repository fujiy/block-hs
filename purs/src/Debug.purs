module Block.Debug where

import Prelude
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff.Console

foreign import console_log :: forall a. a -> Unit

trace :: forall a b. b -> a -> a
trace b a = const a (console_log b)

-- trace :: forall a. String -> a -> a
-- trace s a = const a (unsafePerformEff $ log s)

traceId :: forall a. a -> a
traceId a = trace a a

traceShow :: forall a b. Show b => b -> a -> a
traceShow b a = trace (show b) a

traceShowId :: forall a. Show a => a -> a
traceShowId a = traceShow a a

module Block.Debug where

import Prelude
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff.Console

foreign import console_log :: forall a. a -> Unit
foreign import console_log_with :: forall a. String -> a -> Unit
foreign import console_error :: forall a. a -> Unit

trace :: forall a b. b -> a -> a
trace b a = const a (console_log b)

-- trace :: forall a. String -> a -> a
-- trace s a = const a (unsafePerformEff $ log s)

traceWith :: forall a b. String -> b -> a -> a
traceWith s b a = const a (console_log_with s b)

traceId :: forall a. a -> a
traceId a = trace a a

traceShow :: forall a b. Show b => b -> a -> a
traceShow b a = trace (show b) a

traceShowId :: forall a. Show a => a -> a
traceShowId a = traceShow a a

throw :: forall a b. b -> a -> a
throw e a = const a (console_error e)

module MonadResult where

import Prelude

class MonadResult m where
  assertEq :: forall v. Eq v => Show v => v -> v -> m Unit

infix 5 assertEq as ===
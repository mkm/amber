module Amber.Util.Reader (
        locals,
    ) where

import Control.Monad.Reader

import Amber.Util.Misc

locals :: (MonadReader r m, Foldable t) => t (r -> r) -> m a -> m a
locals = local . compose

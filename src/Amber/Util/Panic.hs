module Amber.Util.Panic (
        todo,
        impossible,
    ) where

import GHC.Stack

todo :: HasCallStack => a
todo = error "todo"

impossible :: HasCallStack => a
impossible = error "impossible"

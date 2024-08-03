module Amber.Util.Polysemy (
        Eff,
        locals,
        withError,
    ) where

import Polysemy
import Polysemy.Reader
import Polysemy.Error

import Amber.Util.Misc

type Eff es a = forall r. Members es r => Sem r a

locals :: (Foldable t, Member (Reader i) r) => t (i -> i) -> Sem r a -> Sem r a
locals = local . compose

withError :: Member (Error e) r => (e -> e) -> Sem r a -> Sem r a
withError f m = m `catch` \e -> throw (f e)

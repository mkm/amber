module Amber.Util.From (
        From(..),
    ) where

import Data.Text (Text)
import qualified Data.Text as T

class From a b where
    from :: a -> b

    default from :: (a ~ b) => a -> b
    from = id

data Through a b = Through { unThrough :: b }
    deriving (Show, Read, Eq, Ord)

instance From b c => From (Through a b) c where
    from = from . unThrough

instance (From a b, From b c) => From a (Through b c) where
    from x = Through $ from (from x :: b)

instance From Bool Bool where
instance From Int Int where
instance From Integer Integer where

instance From a b => From (Maybe a) (Maybe b) where
    from = fmap from

instance From a b => From [a] [b] where
    from = map from

instance From Int Integer where
    from = fromIntegral

instance From String Text where
    from = T.pack

instance From Text String where
    from = T.unpack

instance From a b => From a (Maybe b) where
    from = Just . from

instance From a b => From a (Either c b) where
    from = Right . from



module Data.VCache.Seq
    ( Seq
    , empty
    , singleton
    , length
    , null
    , toList
    ) where

import Prelude hiding (length, null)
import qualified Data.Foldable as F
import Data.VCache.Seq.Type
import Database.VCache

-- | Model an empty sequence in a given VSpace
empty :: (VCacheable a) => VSpace -> Seq a
empty vc = Seq vc Empty

-- | Create a singleton sequence in a given VSpace
singleton :: (VCacheable a) => VSpace -> a -> Seq a
singleton vc = Seq vc . Single . Elem

-- | Test whether a sequence is empty.
null :: Seq a -> Bool
null = null' . q_data where
    null' Empty = True
    null' _ = False

-- | O(1) return the length of the sequence.
length :: Seq a -> Int
length = sizeOf . q_data

toList :: Seq a -> [a]
toList = F.toList




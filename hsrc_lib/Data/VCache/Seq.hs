

module Data.VCache.Seq
    ( Seq
    , empty
    , length
    , null
    ) where

import Prelude hiding (length, null)
import Data.VCache.Seq.Type
import Database.VCache


-- | Model an empty sequence in a given VSpace
empty :: (VCacheable a) => VSpace -> Seq a
empty vc = Seq vc Empty

-- | Test whether a sequence is empty.
null :: Seq a -> Bool
null = null' . q_data where
    null' Empty = True
    null' _ = False

length :: Seq a -> Int
length = sizeOf . q_data





{-# LANGUAGE DeriveDataTypeable, CPP #-}

-- | internal structures and utilities for Data.VCache.Seq 
module Data.VCache.Seq.Type
    ( Seq(..)
    , Elem(..)
    , FingerTree(..)
    , Digit(..)
    , Node(..)
    , VNode(..)
    , Sized(..)
    , seqErr
    ) where

import Prelude hiding (foldr)
import Control.Applicative
import Data.Foldable
import Data.Typeable (Typeable)
import Database.VCache

-- | Our Sequence type requires O(lg(N)) space in main memory, and has
-- between one to four direct elements in memory at each end of a large
-- sequence. Size of a sequence is computed in O(1) time.
data Seq a = Seq 
    { q_space :: !VSpace
    , q_data  :: !(FingerTree (Elem a))
    } deriving (Typeable)

-- wrapper for the Size class
newtype Elem a = Elem { unElem :: a }
    deriving (Typeable)

-- spine of the finger tree
data FingerTree a 
    = Empty
    | Single a
    | Deep {-# UNPACK #-} !Int !(Digit a) (FingerTree (VNode a)) !(Digit a)
    deriving (Typeable)

-- one to four direct elements
data Digit a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a
    deriving (Typeable)

-- indirection through VCache
data VNode a = VNode 
    { n_size :: {-# UNPACK #-} !Int
    , n_data :: !(VRef (Node a))
    } deriving (Typeable)

-- 2-3 tree structure
data Node a
    = Node2 a a
    | Node3 a a a
    deriving (Typeable)

instance VCacheable a => VCacheable (Seq a) where
    put (Seq vc d) = put vc >> put d
    get = Seq <$> get <*> get
instance VCacheable a => VCacheable (Elem a) where
    put (Elem a) = put a
    get = Elem <$> get
instance VCacheable a => VCacheable (Digit a) where
    put (One a) = putWord8 1 >> put a
    put (Two a b) = putWord8 2 >> put a >> put b
    put (Three a b c) = putWord8 3 >> put a >> put b >> put c
    put (Four a b c d) = putWord8 4 >> put a >> put b >> put c >> put d
    get = getWord8 >>= \ n -> case n of
        1 -> One <$> get
        2 -> Two <$> get <*> get
        3 -> Three <$> get <*> get <*> get
        4 -> Four <$> get <*> get <*> get <*> get
        _ -> fail $ seqErr $ "unexpected number of digits: " ++ show n
instance VCacheable a => VCacheable (Node a) where
    put (Node2 a b) = putWord8 2 >> put a >> put b
    put (Node3 a b c) = putWord8 3 >> put a >> put b >> put c
    get = getWord8 >>= \ n -> case n of
        2 -> Node2 <$> get <*> get
        3 -> Node3 <$> get <*> get <*> get
        _ -> fail $ seqErr $ "unexpected node size: " ++ show n
instance VCacheable a => VCacheable (VNode a) where
    put (VNode sz ref) = putVarNat (toInteger sz) >> put ref
    get = VNode <$> (fromInteger <$> getVarNat) <*> get
instance VCacheable a => VCacheable (FingerTree a) where
    put Empty = putWord8 0
    put (Single a) = putWord8 1 >> put a
    put (Deep n l t r) = putWord8 2 >> putVarNat (toInteger n) >> put l >> put t >> put r
    get = getWord8 >>= \ n -> case n of
        0 -> pure Empty
        1 -> Single <$> get
        2 -> Deep <$> (fromInteger <$> getVarNat) <*> get <*> get <*> get
        _ -> fail $ seqErr $ "unexpected FingerTree variant: " ++ show n


class Sized a where 
    sizeOf :: a -> Int
instance Sized (Elem a) where 
    {-# INLINE sizeOf #-}
    sizeOf _ = 1
instance (Sized a) => Sized (Node a) where 
    sizeOf (Node2 a b) = sizeOf a + sizeOf b
    sizeOf (Node3 a b c) = sizeOf a + sizeOf b + sizeOf c
instance Sized (VNode a) where
    {-# INLINE sizeOf #-}
    sizeOf = n_size
instance Sized a => Sized (Digit a) where
    sizeOf (One a) = sizeOf a
    sizeOf (Two a b) = sizeOf a + sizeOf b
    sizeOf (Three a b c) = sizeOf a + sizeOf b + sizeOf c
    sizeOf (Four a b c d) = sizeOf a + sizeOf b + sizeOf c + sizeOf d
instance Sized a => Sized (FingerTree a) where
    {-# SPECIALIZE instance Sized (FingerTree (Elem a)) #-}
    {-# SPECIALIZE instance Sized (FingerTree (VNode a)) #-}
    sizeOf Empty = 0
    sizeOf (Single x) = sizeOf x
    sizeOf (Deep n _ _ _) = n

instance Foldable Seq where 
    foldr fn acc = foldr (fn . unElem) acc . q_data 
#if MIN_VERSION_base(4,8,0)
    length = sizeOf . q_data
    null = null . q_data 
#endif

-- foldr flipped for the way I prefer to use it
_foldr :: Foldable t => (elem -> accum -> accum) -> t elem -> accum -> accum
_foldr = flip . foldr
{-# INLINE _foldr #-}

instance Foldable FingerTree where
    foldr _ acc Empty = acc
    foldr fn acc (Single a) = fn a acc
    foldr fn acc (Deep _ l t r) = _foldr fn l $ _foldr fn' t $ _foldr fn r acc
        where fn' = _foldr fn . deref' . n_data
#if MIN_VERSION_base(4,8,0)
    length = sizeOf
    null Empty = True
    null _ = False
#endif

instance Foldable Digit where
    foldr fn acc (One a) = fn a acc
    foldr fn acc (Two a b) = fn a $ fn b acc
    foldr fn acc (Three a b c) = fn a $ fn b $ fn c acc
    foldr fn acc (Four a b c d) = fn a $ fn b $ fn c $ fn d acc
instance Foldable Node where
    foldr fn acc (Node2 a b) = fn a $ fn b acc
    foldr fn acc (Node3 a b c) = fn a $ fn b $ fn c acc

seqErr :: String -> String
seqErr = (++) "Data.VCache.Seq: "


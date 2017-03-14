{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LargeInt
-- Copyright   :  (c) Eitan Chatav 2017
-- License     :  BSD
--
-- Maintainer  :  eitan@morphism.tech
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides Int128, Int192 and Int256 and a way of producing other
-- large ints if required.
--
-----------------------------------------------------------------------------

module Data.LargeInt
  ( TwosComplement(..)
  , Int96
  , Int128
  , Int160
  , Int192
  , Int224
  , Int256
  ) where

import Data.Binary (Binary, put, get)
import Data.Bits
import Data.LargeWord
  ( Word96
  , Word128
  , Word160
  , Word192
  , Word224
  , Word256
  )

#if !(MIN_VERSION_base(4,7,0))
class FiniteBits a where
  finiteBitSize :: a -> Int

instance FiniteBits Word8 where
  finiteBitSize = bitSize

instance FiniteBits Word16 where
  finiteBitSize = bitSize

instance FiniteBits Word32 where
  finiteBitSize = bitSize

instance FiniteBits Word64 where
  finiteBitSize = bitSize
#endif

newtype TwosComplement a = TwosComplement { unTwosComplement :: a }
  deriving Eq

instance (Ord a, Num a, FiniteBits a) => FiniteBits (TwosComplement a) where
  finiteBitSize (TwosComplement a) = finiteBitSize a

instance (Ord a, Bits a, FiniteBits a) => Ord (TwosComplement a) where
  compare (TwosComplement a) (TwosComplement b)
    | testBit a (finiteBitSize a - 1) =
        if testBit b (finiteBitSize b - 1)
          then compare a b  -- a and b are negative
          else LT           -- a is neg, b is non-neg
    | testBit b (finiteBitSize b - 1) = GT -- a non-negative, b is negative
    | otherwise = compare a b -- a and b are non-negative

instance (FiniteBits a, Show a, Num a, Bits a, Ord a)
  => Show (TwosComplement a) where
    show i@(TwosComplement a)
      | i < 0 = '-' : show (complement a + 1)
      | otherwise = show a

instance (Num a, Bits a, Ord a, FiniteBits a) => Read (TwosComplement a) where
  readsPrec i s = [(fromInteger i', str) | (i',str) <- readsPrec i s]

instance (FiniteBits a, Num a, Bits a, Ord a) => Num (TwosComplement a) where
  (TwosComplement a) + (TwosComplement b) = TwosComplement (a+b)
  (TwosComplement a) - (TwosComplement b) = TwosComplement (a-b)
  (TwosComplement a) * (TwosComplement b) = TwosComplement (a*b)
  negate (TwosComplement a) = TwosComplement (complement a + 1)
  signum a = if a < 0 then -1 else if a > 0 then 1 else 0
  abs a = if a < 0 then negate a else a
  fromInteger i =
    if i < 0
      then negate (TwosComplement $ fromInteger (abs i))
      else TwosComplement (fromInteger i)

instance (Bits a, Num a, Ord a, FiniteBits a) => Bits (TwosComplement a) where
  rotate (TwosComplement a) i = TwosComplement (rotate a i)
  (.&.) a b = TwosComplement (unTwosComplement a .&. unTwosComplement b)
  (.|.) a b = TwosComplement (unTwosComplement a .|. unTwosComplement b)
  xor a b   = TwosComplement (unTwosComplement a `xor` unTwosComplement b)
  complement = TwosComplement . complement . unTwosComplement
  shiftL a i = TwosComplement . (`shiftL` i) . unTwosComplement $ a
  shiftR a i =
    let
      parityOp =
        if a < 0
          then \x -> foldl setBit x [finiteBitSize a - i' | i' <- [1,2..i]]
          else id
    in
      parityOp
      . TwosComplement
      . (`shiftR` i)
      . unTwosComplement
      $ a
  setBit a i = TwosComplement . (`setBit` i) . unTwosComplement $ a
  bitSize (TwosComplement a) = finiteBitSize a
#if MIN_VERSION_base(4,7,0)
  bitSizeMaybe (TwosComplement a) = Just (finiteBitSize a)
#endif
  isSigned _ = True
#if MIN_VERSION_base(4,6,0)
  bit = TwosComplement . bit
  testBit a i = (`testBit` i) . unTwosComplement $ a
  popCount (TwosComplement a) = popCount a
#endif

instance (Bits a, Ord a, Integral a, Bounded a, Num a, FiniteBits a)
  => Enum (TwosComplement a) where
    toEnum i = fromIntegral i
    fromEnum i = fromIntegral i
    pred a
      | a > minBound = (a - 1)
      | otherwise = error "tried to take `pred' of minBound"
    succ a
      | a < maxBound = (a + 1)
      | otherwise = error "tried to take `succ' of maxBound"

instance (Integral a, Bits a, Bounded a, FiniteBits a)
  => Integral (TwosComplement a) where
    toInteger i@(TwosComplement h) =
      if i >= 0
        then toInteger h
        else negate (toInteger (negate i))
    quotRem a b =
      let
        (TwosComplement ah) = abs a
        (TwosComplement bh) = abs b
        (q1,r1) = quotRem ah bh
      in
        if a < 0 && b < 0
          then (TwosComplement q1, negate $ TwosComplement r1)
          else if a < 0
            then (negate $ TwosComplement q1, negate $ TwosComplement r1)
            else if b < 0
              then (negate $ TwosComplement q1, TwosComplement r1)
              else (TwosComplement q1, TwosComplement r1)

instance (FiniteBits a, Real a, Bounded a, Integral a, Bits a)
  => Real (TwosComplement a) where toRational = fromIntegral

instance (Bounded a, Ord a, Bits a, Num a, FiniteBits a)
  => Bounded (TwosComplement a) where
    minBound =
      let r = fromIntegral (negate (2^ (finiteBitSize r - 1)) :: Integer) in r
    maxBound =
      let r = fromIntegral (2^(finiteBitSize r - 1) - 1 :: Integer) in r

instance Binary a => Binary (TwosComplement a) where
  put = put . unTwosComplement
  get = TwosComplement <$> get

type Int96  = TwosComplement Word96
type Int128 = TwosComplement Word128
type Int160 = TwosComplement Word160
type Int192 = TwosComplement Word192
type Int224 = TwosComplement Word224
type Int256 = TwosComplement Word256

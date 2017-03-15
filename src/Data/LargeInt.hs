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
  ( Two'sComplement(..)
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

newtype Two'sComplement a = Two'sComplement { unTwo'sComplement :: a }
  deriving Eq

instance (Ord a, Num a, FiniteBits a) => FiniteBits (Two'sComplement a) where
  finiteBitSize (Two'sComplement a) = finiteBitSize a

instance (Ord a, Bits a, FiniteBits a) => Ord (Two'sComplement a) where
  compare (Two'sComplement a) (Two'sComplement b)
    | testBit a (finiteBitSize a - 1) =
        if testBit b (finiteBitSize b - 1)
          then compare a b  -- a and b are negative
          else LT           -- a is neg, b is non-neg
    | testBit b (finiteBitSize b - 1) = GT -- a non-negative, b is negative
    | otherwise = compare a b -- a and b are non-negative

instance (FiniteBits a, Show a, Num a, Bits a, Ord a)
  => Show (Two'sComplement a) where
    show i@(Two'sComplement a)
      | i < 0 = '-' : show (complement a + 1)
      | otherwise = show a

instance (Num a, Bits a, Ord a, FiniteBits a) => Read (Two'sComplement a) where
  readsPrec i s = [(fromInteger i', str) | (i',str) <- readsPrec i s]

instance (FiniteBits a, Num a, Bits a, Ord a) => Num (Two'sComplement a) where
  (Two'sComplement a) + (Two'sComplement b) = Two'sComplement (a+b)
  (Two'sComplement a) - (Two'sComplement b) = Two'sComplement (a-b)
  (Two'sComplement a) * (Two'sComplement b) = Two'sComplement (a*b)
  negate (Two'sComplement a) = Two'sComplement (complement a + 1)
  signum a = if a < 0 then -1 else if a > 0 then 1 else 0
  abs a = if a < 0 then negate a else a
  fromInteger i =
    if i < 0
      then negate (Two'sComplement $ fromInteger (abs i))
      else Two'sComplement (fromInteger i)

instance (Bits a, Num a, Ord a, FiniteBits a) => Bits (Two'sComplement a) where
  rotate (Two'sComplement a) i = Two'sComplement (rotate a i)
  (.&.) a b = Two'sComplement (unTwo'sComplement a .&. unTwo'sComplement b)
  (.|.) a b = Two'sComplement (unTwo'sComplement a .|. unTwo'sComplement b)
  xor a b   = Two'sComplement (unTwo'sComplement a `xor` unTwo'sComplement b)
  complement = Two'sComplement . complement . unTwo'sComplement
  shiftL a i = Two'sComplement . (`shiftL` i) . unTwo'sComplement $ a
  shiftR a i =
    let
      parityOp =
        if a < 0
          then \x -> foldl setBit x [finiteBitSize a - i' | i' <- [1,2..i]]
          else id
    in
      parityOp
      . Two'sComplement
      . (`shiftR` i)
      . unTwo'sComplement
      $ a
  setBit a i = Two'sComplement . (`setBit` i) . unTwo'sComplement $ a
  bitSize (Two'sComplement a) = finiteBitSize a
  bitSizeMaybe (Two'sComplement a) = Just (finiteBitSize a)
  isSigned _ = True
  bit = Two'sComplement . bit
  testBit a i = (`testBit` i) . unTwo'sComplement $ a
  popCount (Two'sComplement a) = popCount a

instance (Bits a, Ord a, Integral a, Bounded a, Num a, FiniteBits a)
  => Enum (Two'sComplement a) where
    toEnum i = fromIntegral i
    fromEnum i = fromIntegral i
    pred a
      | a > minBound = (a - 1)
      | otherwise = error "tried to take `pred' of minBound"
    succ a
      | a < maxBound = (a + 1)
      | otherwise = error "tried to take `succ' of maxBound"

instance (Integral a, Bits a, Bounded a, FiniteBits a)
  => Integral (Two'sComplement a) where
    toInteger i@(Two'sComplement h) =
      if i >= 0
        then toInteger h
        else negate (toInteger (negate i))
    quotRem a b =
      let
        (Two'sComplement ah) = abs a
        (Two'sComplement bh) = abs b
        (q1,r1) = quotRem ah bh
      in
        if a < 0 && b < 0
          then (Two'sComplement q1, negate $ Two'sComplement r1)
          else if a < 0
            then (negate $ Two'sComplement q1, negate $ Two'sComplement r1)
            else if b < 0
              then (negate $ Two'sComplement q1, Two'sComplement r1)
              else (Two'sComplement q1, Two'sComplement r1)

instance (FiniteBits a, Real a, Bounded a, Integral a, Bits a)
  => Real (Two'sComplement a) where toRational = fromIntegral

instance (Bounded a, Ord a, Bits a, Num a, FiniteBits a)
  => Bounded (Two'sComplement a) where
    minBound =
      let r = fromIntegral (negate (2^ (finiteBitSize r - 1)) :: Integer) in r
    maxBound =
      let r = fromIntegral (2^(finiteBitSize r - 1) - 1 :: Integer) in r

instance Binary a => Binary (Two'sComplement a) where
  put = put . unTwo'sComplement
  get = Two'sComplement <$> get

type Int96  = Two'sComplement Word96
type Int128 = Two'sComplement Word128
type Int160 = Two'sComplement Word160
type Int192 = Two'sComplement Word192
type Int224 = Two'sComplement Word224
type Int256 = Two'sComplement Word256

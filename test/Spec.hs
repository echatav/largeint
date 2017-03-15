{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -Werror                    #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

module Main (main) where

import Test.HUnit hiding (Test)
import Test.QuickCheck hiding ((.&.))
import Test.Framework ( Test, defaultMain )
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Data.LargeInt
import Data.LargeWord
import Data.Bits
import Control.Monad
import Data.Binary (encode, decode, Binary)
import qualified Data.ByteString.Lazy as LZ


instance (Arbitrary a, Arbitrary b) => Arbitrary (LargeKey a b) where
   arbitrary = liftM2 LargeKey arbitrary arbitrary

instance (Arbitrary a) => Arbitrary (Two'sComplement a) where
  arbitrary = fmap Two'sComplement arbitrary

pShiftRightShiftLeft :: Int128 -> Bool
pShiftRightShiftLeft x =
  abs x > maxBound `div` 2
  || shiftR (shiftL x 1) 1 == x

u1 :: Assertion
u1 = shiftR (18446744073709551616  :: Int128) 64  @?= 1

pQuotRem :: Int256 -> Bool
pQuotRem x = rx == fromInteger ry
  where
    (_qx, rx) = quotRem x 16
    (_qy, ry) = quotRem ((fromIntegral x) :: Integer) 16

encodeDecode :: (Binary a, Eq a) => Two'sComplement a -> Bool
encodeDecode int = decode encoded == int
  where
    encoded = encode int
    {-# NOINLINE encoded #-}

correctEncoding :: Assertion
correctEncoding = (decode . LZ.pack)
  [0,0,0,0,0,0,0,0,50,89,125,125,237,119,73,240
  ,217,12,178,101,235,8,44,221,50,122,244,125,115,181,239,78]
  @?=
    (1234567891234567891234567812345678123456781234567812345678 :: Int256)

pRotateLeftRight :: Int256 -> Bool
pRotateLeftRight x = rotate (rotate x 8) (-8) == x

pRepeatedShift :: Int -> Property
pRepeatedShift n =
  (n >= 0) && (n <= 1024) ==>
  (((iterate (`shift` 8) (1::Int192))!!n) == shift (1::Int192) (n*8))

pRepeatedShift' :: Int -> Property
pRepeatedShift' n =
  (n >= 0) && (n <= 1024) ==>
  (((iterate (`shift` 8) a)!!n) == shift a (n*8))
  where a :: Int192
        a = 0x0123456789ABCDEFFEDCBA98765432100011223344556677

pRepeatedShift160 :: Int -> Property
pRepeatedShift160 n =
  (n >= 0) && (n <= 1024) ==>
  (((iterate (`shift` 8) (1::Int160))!!n) == shift (1::Int160) (n*8))

u2 :: Assertion
u2 = (2 :: Two'sComplement (LargeKey Word256 Word128)) ^ 254 @?=
     (fromInteger (2 :: Integer) ^ 254)

u3 :: Assertion
u3 = rotate (rotate ((2^255) :: Int256) (1)) (-1) @?=
     ((2^255) :: Int256)

u4 :: Assertion
u4 = shift (0x0123456789ABCDEFFEDCBA98765432100011223344556677 :: Int192) 80 @?=
           (0xBA9876543210001122334455667700000000000000000000 :: Int192)

u5 :: Assertion
u5 = shift (0x112233445566778899AABBCC :: Int96) 40 @?=
           (0x66778899AABBCC0000000000 :: Int96)

u6 :: Assertion
u6 = rotate ((2^95) :: Int96) (1) @?= 1

u7 :: Assertion
u7 = (2^64 :: Int128) < (2 :: Int128) @?= False

tests :: [Test]
tests =
    [ testProperty "largeint shift left then right" pShiftRightShiftLeft
    , testProperty "largeint quotRem by 16" pQuotRem
    , testProperty "largeint rotate left then right" pRotateLeftRight
    , testProperty "largeint repeated shift vs single shift" pRepeatedShift
    , testProperty "largeint repeated shift vs single shift" pRepeatedShift'
    , testProperty "largeint repeated shift vs single shift" pRepeatedShift160
    , testCase "largeint shift 2^64 by 2^64" u1
    , testCase "largeint exponentiation 2^254" u2
    , testCase "largeint rotation by 1" u3
    , testCase "largeint shift by 80" u4
    , testCase "largeint shift by 40" u5
    , testCase "largeint rotate by 1" u6
    , testCase "big-endian encoding" correctEncoding
    , testProperty "Int96 encode/decode loop" (encodeDecode::Int96 -> Bool)
    , testProperty "Int128 encode/decode loop" (encodeDecode::Int128 -> Bool)
    , testProperty "Int160 encode/decode loop" (encodeDecode::Int160 -> Bool)
    , testProperty "Int192 encode/decode loop" (encodeDecode::Int192 -> Bool)
    , testProperty "Int224 encode/decode loop" (encodeDecode::Int224 -> Bool)
    , testProperty "Int256 encode/decode loop" (encodeDecode::Int256 -> Bool)
    , testCase "largeint Ord test" u7
    ]

main :: IO ()
main = defaultMain tests

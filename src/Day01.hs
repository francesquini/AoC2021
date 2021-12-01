{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Data.Foldable
import GHC.TypeNats
import Data.Ord
import Data.List
import Data.Proxy

type SlidingWindow :: Nat -> *
newtype SlidingWindow n = SW [Int] deriving Show

intNatVal :: KnownNat n => t n -> Int
intNatVal = fromIntegral . natVal

normalize :: KnownNat n => SlidingWindow n -> SlidingWindow n
normalize sw@(SW xs) = SW $ drop (length xs - intNatVal sw) xs

mkSw :: KnownNat n => [Int] -> SlidingWindow n
mkSw = normalize . SW

swSum :: SlidingWindow n -> Int
swSum (SW xs) = sum xs

instance Eq (SlidingWindow n) where
  sw1 == sw2 = swSum sw1 == swSum sw2

instance Ord (SlidingWindow n) where
  compare = comparing swSum

instance KnownNat n => Semigroup (SlidingWindow n) where
  (SW xs) <> (SW ys) = mkSw (xs <> ys)

type SMono :: Nat -> *
data SMono n = SMono {
    increases :: Int
  , last      :: SlidingWindow n
  } deriving Show

instance KnownNat n => Semigroup (SMono n) where
  (SMono c0 l0) <> (SMono c1 l1) =
    SMono (c0 + c1 + fromEnum (l1 > l0)) (l0 <> l1)

instance KnownNat n => Monoid (SMono n) where
  mempty = SMono 0 (SW mempty)

probSz :: forall n. KnownNat n => [Int] -> Int
probSz input = increases fld - 1
  where
    fld = foldMap' (SMono 0 . mkSw @n) $ drop (intNatVal $ Proxy @n) (inits input)

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input.01"
  print $ probSz @1 input
  print $ probSz @3 input

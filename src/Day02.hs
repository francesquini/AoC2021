{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import Data.Proxy ( Proxy(..) )
import GHC.TypeNats ( Nat, type (+), type (*), type (-), natVal )
import GHC.TypeLits (Symbol)

type SampleInput = '[
    '("forward", 5)
  , '("down", 5)
  , '("forward", 8)
  , '("up", 3)
  , '("down", 8)
  , '("forward", 2)]

type FullInput = '[
  '("forward", 7), '("forward", 9), '("forward", 9), '("down", 3),
  '("down", 8), '("down", 3), '("forward", 6), '("down", 7), '("up", 3),
  '("forward", 3), '("up", 3), '("down", 3), '("down", 9),
  '("down", 6), '("forward", 8), '("down", 3), '("down", 9),
  '("forward", 4), '("forward", 3), '("down", 1), '("forward", 2),
  '("down", 5), '("forward", 9), '("forward", 6), '("up", 1),
  '("forward", 8), '("down", 9), '("forward", 8), '("down", 7),
  '("down", 9), '("down", 4), '("down", 2), '("forward", 2), '("down", 6),
  '("up", 8), '("forward", 3), '("forward", 7), '("down", 2),
  '("forward", 4), '("down", 3), '("down", 1), '("down", 8),
  '("forward", 5), '("forward", 6), '("down", 3), '("forward", 1),
  '("up", 1), '("down", 8), '("forward", 5), '("forward", 2),
  '("forward", 9), '("up", 4), '("down", 1), '("down", 3), '("up", 5),
  '("up", 5), '("forward", 7), '("up", 1), '("forward", 8),
  '("forward", 4), '("forward", 3), '("down", 9), '("forward", 2),
  '("forward", 5), '("down", 5), '("up", 7), '("forward", 4), '("up", 5),
  '("down", 5), '("down", 4), '("down", 9), '("forward", 1),
  '("up", 3), '("forward", 6), '("down", 3), '("forward", 9),
  '("down", 1), '("up", 5), '("down", 7), '("forward", 3),
  '("forward", 3), '("up", 9), '("down", 2), '("down", 4),
  '("forward", 1), '("forward", 2), '("down", 4), '("down", 4),
  '("up", 5), '("forward", 9), '("down", 3), '("down", 1), '("down", 8),
  '("down", 2), '("down", 8), '("forward", 9), '("forward", 5),
  '("down", 2), '("forward", 8), '("up", 9), '("down", 8), '("down", 5),
  '("down", 6), '("down", 2), '("down", 4), '("up", 2),
  '("forward", 4), '("down", 9), '("forward", 4), '("down", 6),
  '("down", 9), '("forward", 9), '("down", 3), '("down", 1), '("up", 2),
  '("down", 6), '("forward", 2), '("down", 4), '("down", 6),
  '("forward", 5), '("up", 2), '("up", 8), '("down", 2), '("up", 9),
  '("forward", 6), '("down", 8), '("down", 7), '("forward", 3),
  '("forward", 5), '("forward", 4), '("down", 5), '("forward", 2),
  '("up", 1), '("up", 3), '("up", 7), '("forward", 2), '("forward", 2),
  '("up", 6), '("down", 3), '("down", 7), '("down", 8),
  '("forward", 5), '("up", 5), '("down", 9), '("forward", 7),
  '("down", 2), '("down", 6), '("forward", 8), '("down", 1), '("down", 7),
  '("down", 2), '("forward", 2), '("forward", 4), '("down", 5),
  '("up", 5), '("down", 6), '("down", 5), '("down", 8), '("forward", 5),
  '("forward", 4), '("down", 3), '("up", 7), '("up", 4),
  '("forward", 7), '("down", 9), '("down", 3), '("down", 7),
  '("forward", 6), '("up", 4), '("down", 2), '("down", 9), '("up", 1),
  '("down", 2), '("up", 7), '("down", 7), '("down", 1), '("down", 5),
  '("forward", 9), '("down", 5), '("down", 8), '("down", 4), '("down", 5),
  '("forward", 3), '("down", 5), '("forward", 5), '("down", 6),
  '("forward", 6), '("up", 7), '("down", 7), '("down", 3), '("down", 8),
  '("down", 8), '("up", 5), '("down", 3), '("up", 4), '("down", 1),
  '("up", 4), '("up", 2), '("up", 2), '("forward", 6),
  '("forward", 3), '("forward", 4), '("down", 9), '("up", 2),
  '("forward", 2), '("down", 2), '("down", 6), '("up", 2),
  '("forward", 1), '("up", 4), '("forward", 5), '("up", 5),
  '("forward", 3), '("forward", 7), '("down", 2), '("up", 6),
  '("forward", 1), '("up", 6), '("down", 8), '("forward", 1),
  '("forward", 7), '("up", 3), '("up", 5), '("forward", 4), '("down", 1),
  '("forward", 7), '("up", 7), '("forward", 5), '("forward", 6),
  '("forward", 6), '("forward", 2), '("down", 6), '("down", 1),
  '("forward", 4), '("forward", 2), '("forward", 2), '("up", 2),
  '("forward", 1), '("down", 2), '("down", 8), '("forward", 5),
  '("forward", 3), '("up", 1), '("forward", 5), '("down", 8),
  '("down", 6), '("down", 1), '("up", 2), '("down", 1), '("down", 3),
  '("forward", 3), '("up", 9), '("forward", 5), '("down", 6), '("up", 8),
  '("down", 9), '("up", 4), '("down", 8), '("forward", 2),
  '("forward", 9), '("forward", 6), '("forward", 2), '("up", 5),
  '("down", 5), '("down", 6), '("forward", 2), '("forward", 3),
  '("forward", 5), '("forward", 7), '("down", 8), '("forward", 1),
  '("forward", 1), '("forward", 4), '("forward", 5), '("down", 4),
  '("forward", 6), '("forward", 4), '("forward", 6), '("down", 5),
  '("down", 8), '("down", 7), '("up", 9), '("down", 8), '("forward", 2),
  '("down", 9), '("forward", 3), '("down", 4), '("up", 1),
  '("down", 6), '("forward", 4), '("down", 9), '("down", 5), '("down", 3),
  '("up", 6), '("down", 1), '("down", 3), '("forward", 2),
  '("down", 7), '("down", 9), '("forward", 6), '("forward", 1),
  '("forward", 8), '("down", 6), '("down", 2), '("down", 2),
  '("forward", 7), '("up", 8), '("forward", 5), '("forward", 7),
  '("forward", 7), '("forward", 3), '("up", 3), '("forward", 3),
  '("up", 4), '("down", 5), '("forward", 1), '("forward", 7),
  '("forward", 7), '("down", 9), '("up", 4), '("forward", 1),
  '("down", 9), '("forward", 3), '("forward", 8), '("down", 3),
  '("forward", 6), '("down", 7), '("down", 2), '("forward", 1),
  '("down", 4), '("down", 6), '("down", 2), '("forward", 7),
  '("forward", 7), '("down", 9), '("forward", 7), '("down", 8),
  '("forward", 7), '("down", 5), '("down", 7), '("forward", 1),
  '("forward", 7), '("up", 6), '("down", 5), '("forward", 9),
  '("down", 2), '("forward", 7), '("up", 4), '("down", 1), '("down", 7),
  '("forward", 4), '("down", 2), '("down", 4), '("forward", 4),
  '("down", 8), '("forward", 3), '("forward", 2), '("down", 3),
  '("down", 4), '("forward", 2), '("forward", 6), '("down", 9),
  '("down", 3), '("forward", 9), '("up", 4), '("up", 1), '("forward", 7),
  '("up", 3), '("up", 8), '("down", 2), '("up", 2), '("up", 6),
  '("forward", 8), '("down", 3), '("up", 4), '("up", 6), '("down", 8),
  '("forward", 5), '("down", 1), '("up", 2), '("up", 5), '("forward", 7),
  '("down", 6), '("up", 4), '("forward", 1), '("up", 9),
  '("forward", 8), '("down", 3), '("up", 8), '("down", 3), '("down", 5),
  '("up", 6), '("up", 8), '("down", 2), '("up", 4), '("down", 1),
  '("down", 2), '("forward", 4), '("up", 9), '("up", 1), '("up", 2),
  '("forward", 4), '("forward", 9), '("up", 8), '("forward", 8),
  '("down", 8), '("up", 9), '("forward", 3), '("forward", 2),
  '("down", 7), '("up", 1), '("up", 8), '("up", 1), '("up", 6),
  '("up", 9), '("forward", 6), '("forward", 8), '("forward", 8),
  '("forward", 6), '("down", 2), '("down", 4), '("forward", 8),
  '("up", 3), '("forward", 1), '("down", 3), '("down", 1), '("down", 9),
  '("up", 9), '("down", 4), '("forward", 7), '("down", 3),
  '("forward", 1), '("down", 6), '("forward", 7), '("forward", 3),
  '("forward", 2), '("down", 5), '("forward", 6), '("up", 8),
  '("forward", 6), '("forward", 8), '("down", 1), '("up", 3),
  '("down", 1), '("down", 7), '("up", 2), '("down", 8), '("forward", 7),
  '("down", 1), '("forward", 5), '("forward", 5), '("up", 9),
  '("down", 1), '("forward", 6), '("up", 1), '("forward", 1),
  '("down", 8), '("down", 5), '("forward", 7), '("forward", 5),
  '("forward", 3), '("down", 2), '("down", 4), '("forward", 3),
  '("forward", 2), '("forward", 3), '("down", 1), '("down", 6),
  '("down", 8), '("down", 5), '("forward", 6), '("forward", 5),
  '("down", 8), '("forward", 2), '("forward", 6), '("forward", 1),
  '("down", 3), '("down", 7), '("down", 1), '("down", 1), '("down", 6),
  '("down", 5), '("forward", 1), '("up", 9), '("down", 4),
  '("down", 8), '("forward", 4), '("down", 3), '("down", 6), '("down", 9),
  '("up", 4), '("forward", 6), '("down", 1), '("down", 3), '("up", 5),
  '("up", 7), '("forward", 6), '("up", 6), '("down", 6), '("down", 2),
  '("up", 1), '("forward", 1), '("down", 3), '("forward", 2),
  '("down", 6), '("down", 5), '("down", 1), '("down", 5), '("up", 2),
  '("down", 3), '("up", 1), '("forward", 7), '("down", 3),
  '("forward", 5), '("down", 4), '("up", 1), '("down", 7),
  '("forward", 3), '("up", 9), '("down", 2), '("up", 5), '("forward", 7),
  '("up", 8), '("forward", 8), '("forward", 8), '("up", 8),
  '("forward", 8), '("forward", 1), '("forward", 2), '("down", 8),
  '("forward", 6), '("down", 3), '("down", 9), '("forward", 9),
  '("forward", 4), '("down", 6), '("down", 4), '("forward", 3),
  '("up", 2), '("up", 7), '("down", 9), '("down", 2), '("forward", 8),
  '("down", 2), '("down", 3), '("down", 7), '("forward", 9), '("down", 4),
  '("up", 3), '("down", 4), '("down", 5), '("forward", 9),
  '("down", 9), '("forward", 4), '("forward", 3), '("down", 6),
  '("forward", 4), '("down", 5), '("down", 2), '("forward", 1),
  '("down", 4), '("forward", 2), '("up", 3), '("up", 3), '("forward", 4),
  '("down", 4), '("up", 2), '("up", 8), '("forward", 8), '("down", 5),
  '("down", 8), '("down", 7), '("down", 7), '("forward", 6),
  '("forward", 5), '("up", 4), '("down", 6), '("down", 9),
  '("forward", 1), '("down", 3), '("forward", 8), '("down", 4),
  '("forward", 3), '("down", 7), '("forward", 3), '("forward", 3),
  '("forward", 1), '("forward", 5), '("down", 7), '("forward", 4),
  '("up", 5), '("down", 9), '("down", 3), '("down", 6), '("down", 6),
  '("forward", 1), '("down", 2), '("forward", 8), '("forward", 5),
  '("forward", 9), '("up", 2), '("forward", 5), '("down", 1), '("up", 2),
  '("forward", 7), '("down", 9), '("down", 2), '("up", 9),
  '("down", 6), '("forward", 4), '("down", 8), '("forward", 4),
  '("down", 7), '("down", 6), '("up", 7), '("up", 3), '("forward", 6),
  '("forward", 7), '("down", 4), '("down", 6), '("up", 1),
  '("forward", 6), '("down", 6), '("forward", 5), '("forward", 9),
  '("forward", 5), '("forward", 7), '("down", 3), '("down", 9),
  '("forward", 7), '("forward", 6), '("down", 2), '("down", 9),
  '("down", 8), '("down", 5), '("forward", 3), '("up", 5), '("down", 4),
  '("forward", 5), '("down", 8), '("forward", 8), '("up", 3),
  '("down", 5), '("up", 8), '("down", 5), '("down", 1), '("down", 3),
  '("down", 1), '("down", 8), '("up", 9), '("forward", 1),
  '("forward", 5), '("forward", 9), '("forward", 9), '("down", 6),
  '("forward", 3), '("up", 6), '("up", 2), '("down", 6), '("forward", 4),
  '("down", 7), '("up", 1), '("up", 3), '("down", 3), '("up", 3),
  '("up", 1), '("down", 9), '("down", 2), '("up", 6), '("down", 6),
  '("forward", 8), '("forward", 3), '("forward", 1), '("up", 9),
  '("down", 2), '("forward", 6), '("down", 1), '("forward", 5),
  '("up", 8), '("down", 2), '("forward", 7), '("forward", 2),
  '("down", 1), '("forward", 6), '("up", 7), '("down", 7),
  '("forward", 3), '("down", 8), '("down", 6), '("forward", 3),
  '("down", 9), '("down", 7), '("forward", 5), '("forward", 5),
  '("down", 1), '("forward", 3), '("up", 8), '("forward", 8),
  '("down", 3), '("up", 4), '("up", 6), '("up", 5), '("up", 6),
  '("down", 8), '("up", 9), '("down", 6), '("up", 5), '("down", 6),
  '("forward", 4), '("down", 7), '("forward", 5), '("forward", 2),
  '("down", 1), '("down", 5), '("forward", 4), '("forward", 4),
  '("down", 9), '("forward", 6), '("down", 4), '("forward", 1),
  '("forward", 3), '("down", 5), '("forward", 2), '("up", 3),
  '("forward", 6), '("down", 2), '("up", 5), '("down", 6), '("down", 6),
  '("forward", 3), '("up", 5), '("forward", 4), '("forward", 3),
  '("forward", 6), '("forward", 5), '("forward", 7), '("down", 5),
  '("down", 8), '("up", 6), '("up", 3), '("down", 1), '("forward", 6),
  '("down", 4), '("forward", 7), '("up", 3), '("forward", 9),
  '("down", 3), '("forward", 2), '("forward", 8), '("down", 9),
  '("down", 2), '("up", 8), '("down", 3), '("down", 6), '("forward", 8),
  '("forward", 6), '("up", 5), '("forward", 9), '("forward", 7),
  '("down", 9), '("forward", 5), '("down", 1), '("up", 5), '("down", 4),
  '("up", 2), '("forward", 1), '("up", 9), '("forward", 1),
  '("forward", 2), '("down", 1), '("forward", 5), '("forward", 8),
  '("down", 8), '("up", 3), '("down", 4), '("forward", 6), '("down", 9),
  '("down", 9), '("forward", 7), '("forward", 3), '("down", 3),
  '("down", 7), '("forward", 8), '("forward", 1), '("forward", 6),
  '("down", 8), '("up", 2), '("up", 6), '("forward", 2), '("down", 6),
  '("up", 3), '("down", 5), '("forward", 9), '("down", 6), '("down", 9),
  '("down", 8), '("down", 5), '("forward", 3), '("forward", 3),
  '("down", 1), '("forward", 7), '("forward", 5), '("down", 3),
  '("down", 1), '("down", 9), '("forward", 4), '("up", 9), '("up", 8),
  '("down", 2), '("down", 1), '("forward", 6), '("forward", 5),
  '("up", 8), '("up", 4), '("down", 7), '("forward", 9), '("forward", 9),
  '("up", 4), '("forward", 3), '("down", 6), '("forward", 1),
  '("down", 2), '("forward", 2), '("down", 6), '("forward", 4),
  '("down", 2), '("forward", 9), '("down", 3), '("down", 4),
  '("forward", 9), '("forward", 4), '("forward", 7), '("forward", 8),
  '("down", 7), '("up", 5), '("forward", 3), '("forward", 1),
  '("down", 3), '("forward", 2), '("forward", 5), '("forward", 5),
  '("down", 5), '("down", 5), '("down", 6), '("forward", 3), '("up", 4),
  '("forward", 6), '("down", 1), '("down", 3), '("down", 5),
  '("down", 6), '("up", 1), '("forward", 3), '("forward", 2), '("up", 9),
  '("forward", 6), '("up", 1), '("down", 9), '("up", 3), '("down", 4),
  '("forward", 1), '("down", 4), '("forward", 3), '("forward", 5),
  '("down", 2), '("forward", 5), '("down", 5), '("down", 9), '("down", 8),
  '("forward", 5), '("forward", 7), '("down", 1), '("forward", 2),
  '("up", 5), '("up", 9), '("forward", 4), '("up", 7), '("down", 9),
  '("forward", 9), '("up", 9), '("down", 5), '("up", 8), '("forward", 1),
  '("forward", 8), '("up", 9), '("down", 1), '("down", 3),
  '("down", 6), '("forward", 8), '("forward", 9), '("down", 7),
  '("forward", 8), '("down", 9), '("forward", 7), '("up", 6),
  '("forward", 7), '("forward", 3), '("forward", 5), '("down", 4),
  '("forward", 3), '("up", 8), '("up", 3), '("down", 7), '("up", 3),
  '("forward", 6), '("down", 7), '("forward", 5), '("down", 5),
  '("forward", 8), '("down", 4), '("up", 1), '("up", 1), '("forward", 9),
  '("forward", 8), '("up", 3), '("down", 5), '("forward", 8),
  '("forward", 6), '("forward", 9), '("forward", 5), '("forward", 3),
  '("down", 5), '("forward", 4), '("down", 6), '("forward", 9),
  '("up", 6), '("down", 3), '("down", 8), '("down", 5), '("down", 8),
  '("down", 4), '("down", 5), '("down", 2), '("down", 4), '("down", 8),
  '("down", 2), '("down", 7), '("down", 9), '("down", 5), '("up", 3),
  '("down", 1), '("forward", 6), '("forward", 4), '("forward", 1),
  '("forward", 6), '("forward", 4), '("down", 1), '("forward", 3),
  '("forward", 1), '("forward", 1), '("down", 5), '("down", 4),
  '("up", 6), '("forward", 2), '("up", 6), '("down", 8), '("forward", 1),
  '("up", 7), '("down", 4), '("up", 2), '("down", 3), '("forward", 6),
  '("forward", 2), '("up", 5), '("forward", 7), '("down", 8),
  '("forward", 6), '("up", 6), '("down", 9), '("down", 6), '("down", 5),
  '("down", 8), '("down", 5), '("down", 8), '("down", 2),
  '("down", 7), '("up", 6), '("up", 9), '("down", 3), '("down", 5),
  '("forward", 3), '("up", 7), '("down", 7), '("up", 8), '("forward", 4),
  '("forward", 2), '("down", 1), '("up", 1), '("up", 6), '("up", 2),
  '("down", 1), '("down", 6), '("down", 1), '("forward", 4),
  '("down", 6), '("forward", 9), '("forward", 2), '("forward", 1),
  '("forward", 7), '("forward", 5)]

type InitialPosition = '(0, 0, 0)

type Move :: (Nat, Nat, Nat) -> (Symbol, Nat) -> (Nat, Nat, Nat)
type family Move m p where
  Move '(x, y, aim) '("forward", n) = '(x + n, y    , aim)
  Move '(x, y, aim) '("down"   , n) = '(x    , y + n, aim)
  Move '(x, y, aim) '("up"     , n) = '(x    , y - n, aim)

type Move2 :: (Nat, Nat, Nat) -> (Symbol, Nat) -> (Nat, Nat, Nat)
type family Move2 m p where
  Move2 '(x, y, aim) '("forward", n) = '(x + n, y + aim * n, aim)
  Move2 '(x, y, aim) '("down"   , n) = '(x    , y          , aim + n)
  Move2 '(x, y, aim) '("up"     , n) = '(x    , y          , aim - n)

type FoldPos :: b -> [a] -> b
type family FoldPos x as where
  FoldPos x '[] = x
  FoldPos x (a ': as) = FoldPos (Move x a) as

type FoldPos2 :: b -> [a] -> b
type family FoldPos2 x as where
  FoldPos2 x '[] = x
  FoldPos2 x (a ': as) = FoldPos2 (Move2 x a) as

type Mult :: (Nat, Nat, Nat) -> Nat
type family Mult p where
  Mult '(x, y, _) = x * y

type Prob01 input = Mult (FoldPos InitialPosition input)
type Prob02 input = Mult (FoldPos2 InitialPosition input)

main :: IO ()
main = do
  putStrLn "Problem 01"
  print $ natVal (Proxy @(Prob01 SampleInput))
  print $ natVal (Proxy @(Prob01 FullInput))

  putStrLn "Problem 02"
  print $ natVal (Proxy @(Prob02 SampleInput))
  print $ natVal (Proxy @(Prob02 FullInput))

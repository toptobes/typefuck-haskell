{-# LANGUAGE UndecidableInstances #-}

module TF.Utils where
  
import GHC.TypeLits (ConsSymbol, Symbol, UnconsSymbol, type(-), type(+), Mod)

type SCons = Maybe (Char, Symbol)

type family GetIdx (xs :: [a]) (i :: Nat) :: a where
  GetIdx (x:xs) 0 = x
  GetIdx (x:xs) i = GetIdx xs (i - 1)

type family SetIdx (xs :: [a]) (i :: Nat) (v :: a) :: [a] where
  SetIdx (x:xs) 0 v = v : xs
  SetIdx (x:xs) i v = x : SetIdx xs (i - 1) v

type family Listify (s :: Symbol) :: [Char] where
  Listify s = Listify' (UnconsSymbol (Reverse s)) '[]

type family Listify' (s :: SCons) (acc :: [Char]) :: [Char] where
  Listify' ('Just '(first, rest)) acc = Listify' (UnconsSymbol rest) (first:acc)
  Listify' 'Nothing acc = acc

type family Reverse (sym :: Symbol) :: Symbol where
  Reverse sym = Reverse' "" (UnconsSymbol sym)

type family Reverse' (acc :: Symbol) (xs :: SCons) :: Symbol where
  Reverse' acc 'Nothing = acc
  Reverse' acc ('Just '(x, xs)) = Reverse' (ConsSymbol x acc) (UnconsSymbol xs)

type family (a :: Nat) -~ (b :: Nat) :: Nat where
  a -~ b = UnderflowingSub' (CmpNat a b) a b

type family UnderflowingSub' (ord :: Ordering) (a :: Nat) (b :: Nat) where
  UnderflowingSub' 'LT a b = 256 - (b - a)
  UnderflowingSub'  _  a b = a - b

type family (a :: Nat) +~ (b :: Nat) :: Nat where
  a +~ b = Mod (a + b) 256

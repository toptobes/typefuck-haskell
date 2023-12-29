{-# LANGUAGE UndecidableInstances, PolyKinds #-}

module TF.BracketLUT
  ( BracketLUT
  , MkLUT
  , DistToClose
  , DistToOpen
  ) where

import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError, UnconsSymbol, type(+), type(-))
import TF.Utils (SCons)

type BracketLUT = [(Nat, Nat)]

type Opn = '['
type Cls = ']'

type family MkLUT (code :: Symbol) where
  MkLUT code = MkLUT' code 0 '[] '[]

type family MkLUT' (code :: Symbol) (idx :: Nat) (lut :: BracketLUT) (stack :: [Nat]) :: BracketLUT where
  MkLUT' code idx lut stack = MkLUT'' (UnconsSymbol code) idx lut stack

type family MkLUT'' (code :: SCons) (idx :: Nat) (lut :: BracketLUT) (stack :: [Nat]) :: BracketLUT where
  MkLUT'' ('Just '(Opn, rest)) idx lut stack = MkLUT' rest (idx + 1) lut (idx:stack)
  MkLUT'' ('Just '(Cls, rest)) idx lut (match:stack) = MkLUT' rest (idx + 1) ('(match, idx):lut) stack
  MkLUT'' ('Just '(Cls, rest)) idx lut '[] = TypeError ('Text "Mismatched brackets")
  MkLUT'' ('Just '(op,  rest)) idx lut stack = MkLUT' rest (idx + 1) lut stack
  MkLUT'' 'Nothing _ lut '[] = lut
  MkLUT'' 'Nothing _ _ stack = TypeError ('Text "Mismatched brackets")

type family DistToClose (lut :: BracketLUT) (idx :: Nat) :: Nat where
  DistToClose ('(open, close):xs) idx = DistToClose' (CmpNat open idx) xs idx open close

type family DistToClose' (ord :: Ordering) (rest :: BracketLUT) (idx :: Nat) (open :: Nat) (close :: Nat) where
  DistToClose' 'EQ rest idx open close = (close - open)
  DistToClose'  _  rest idx open close = (DistToClose rest idx)

type family DistToOpen (lut :: BracketLUT) (idx :: Nat) :: Nat where
  DistToOpen ('(open, close):xs) idx = DistToOpen' (CmpNat close idx) xs idx open close

type family DistToOpen' (ord :: Ordering) (rest :: BracketLUT) (idx :: Nat) (open :: Nat) (close :: Nat) where
  DistToOpen' 'EQ rest idx open close = (close - open)
  DistToOpen'  _  rest idx open close = (DistToOpen rest idx)

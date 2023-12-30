{-# LANGUAGE UndecidableInstances #-}

module TF.Cursor where

import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError, UnconsSymbol, type(+), type(-))
import TF.Utils (Listify, SCons)

type Cursor a = (Nat, [a], a, [a])

type OpsCursor = Cursor Char
type MemCursor = Cursor Nat

type family MkOpsCursor (ops :: Symbol) :: OpsCursor where
  MkOpsCursor ops = MkOpsCursor' (UnconsSymbol ops)

type family MkOpsCursor' (ops :: SCons) :: OpsCursor where
  MkOpsCursor' ('Just '(first, rest)) = '(0, '[], first, Listify rest)
  MkOpsCursor' 'Nothing = (TypeError ('Text "No code provided"))

type family MkMemCursor (xs :: [Nat]) :: MemCursor where
  MkMemCursor (first:rest) = '(0, '[], first, rest)

type family SlideR (code :: Cursor a) (n :: Nat) :: Cursor a where
  SlideR code 0 = code
  SlideR '(_, _, _, '[]) n = TypeError ('Text "Can't slide right any further")
  SlideR '(idx, as, current, b:bs) n = SlideR '(idx + 1, current:as, b, bs) (n - 1)

type family SlideL (code :: Cursor a) (n :: Nat) :: Cursor a where
  SlideL code 0 = code
  SlideL '(_, '[], _, _) n = TypeError ('Text "Can't slide left any further")
  SlideL '(idx, a:as, current, bs) n = SlideL '(idx - 1, as, a, current:bs) (n - 1)

type family GetCursor (code :: Cursor a) :: a where
  GetCursor '(_, _, current, _) = current 

type family SetCursor (code :: Cursor a) (v :: a) :: Cursor a where
  SetCursor '(ptr, before, _, after) v = '(ptr, before, v, after) 

type family Idx (code :: Cursor a) :: Nat where
  Idx '(idx, _, _, _) = idx

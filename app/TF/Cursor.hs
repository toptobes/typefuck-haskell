{-# LANGUAGE UndecidableInstances #-}

module TF.Cursor where

import GHC.TypeLits (Symbol, UnconsSymbol, type(+), type(-))
import TF.Utils (Listify, SCons)

type Cursor = (Nat, [Char], Char, [Char])

type family MkCursor (sym :: Symbol) :: Cursor where
  MkCursor code = MkCursor' (UnconsSymbol code)

type family MkCursor' (sym :: SCons) :: Cursor where
  MkCursor' ('Just '(first, rest)) = '(0, '[], first, Listify rest)

type family SlideR (code :: Cursor) (n :: Nat) :: Cursor where
  SlideR code 0 = code
  SlideR '(idx, as, current, b:bs) n = SlideR '(idx + 1, current:as, b, bs) (n - 1)

type family SlideL (code :: Cursor) (n :: Nat) :: Cursor where
  SlideL code 0 = code
  SlideL '(idx, a:as, current, bs) n = SlideL '(idx - 1, as, a, current:bs) (n - 1)

type family Current (code :: Cursor) :: Char where
  Current '(_, _, current, _) = current 

type family Idx (code :: Cursor) :: Nat where
  Idx '(idx, _, _, _) = idx

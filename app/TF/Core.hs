{-# LANGUAGE UndecidableInstances #-}

module TF.Core where

import GHC.TypeLits
import TF.Utils
import TF.Cursor
import TF.BracketLUT
import TF.Generated.Tape

type TFState = (Cursor, [Nat], Nat, Symbol, Symbol, BracketLUT)

type family InitTF (code :: Symbol) (i :: Symbol) :: TFState where
  InitTF code i = '(MkCursor code, Tape, 0, i, "", MkLUT code)

type RunTF code input = InterpNext' (InitTF code input)

type family InterpNext (state :: TFState) :: Symbol where
  InterpNext '( '(_, _, _, '[]), tape, ptr, i, o, lut) = Reverse o
  InterpNext '(code, tape, ptr, i, o, lut) = InterpNext' '(SlideR code 1, tape, ptr, i, o, lut)

type family InterpNext' (state :: TFState) :: Symbol where
  InterpNext' '(code, tape, ptr, i, o, lut) = InterpNext'' (Current code) '(code, tape, ptr, i, o, lut)

type family InterpNext'' (op :: Char) (state :: TFState) :: Symbol where
  InterpNext'' '>' '(code, tape, ptr, i, o, lut) = InterpNext '(code, tape, ptr + 1, i, o, lut)
  InterpNext'' '<' '(code, tape, ptr, i, o, lut) = InterpNext '(code, tape, ptr - 1, i, o, lut)

  InterpNext'' '+' '(code, tape, ptr, i, o, lut) = InterpNext '(code, SetIdx tape ptr (GetIdx tape ptr +~ 1), ptr, i, o, lut)
  InterpNext'' '-' '(code, tape, ptr, i, o, lut) = InterpNext '(code, SetIdx tape ptr (GetIdx tape ptr -~ 1), ptr, i, o, lut)

  InterpNext'' '.' '(code, tape, ptr, i, o, lut) = InterpNext '(code, tape, ptr, i, ConsSymbol (NatToChar (GetIdx tape ptr)) o, lut)
  InterpNext'' ',' '(code, tape, ptr, i, o, lut) = StoreInput (UnconsSymbol i) '(code, tape, ptr, i, o, lut)

  InterpNext'' '[' '(code, tape, ptr, i, o, lut) = OnOpen  (CmpNat (GetIdx tape ptr) 0) '(code, tape, ptr, i, o, lut)
  InterpNext'' ']' '(code, tape, ptr, i, o, lut) = OnClose (CmpNat (GetIdx tape ptr) 0) '(code, tape, ptr, i, o, lut)

  InterpNext'' _ state = InterpNext state

type family StoreInput (i :: SCons) (state :: TFState) :: Symbol where
  StoreInput ('Just '(char, rest)) '(code, tape, ptr, i, o, lut) = InterpNext '(code, SetIdx tape ptr (CharToNat char), ptr, rest, o, lut)
  StoreInput 'Nothing _ = TypeError ('Text "Ran out of input...")

type family OnOpen (ord :: Ordering) (state :: TFState) where
  OnOpen 'EQ '(code, tape, ptr, i, o, lut) = InterpNext '(SlideR code (DistToClose lut (Idx code)), tape, ptr, i, o, lut)
  OnOpen _ state = InterpNext state

type family OnClose (ord :: Ordering) (state :: TFState) where
  OnClose 'EQ state = InterpNext state
  OnClose _ '(code, tape, ptr, i, o, lut) = InterpNext '(SlideL code (DistToOpen lut (Idx code)), tape, ptr, i, o, lut)

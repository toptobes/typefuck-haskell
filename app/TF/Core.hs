{-# LANGUAGE UndecidableInstances #-}

module TF.Core where

import GHC.TypeLits
import TF.Utils
import TF.Cursor
import TF.BracketLUT
import TF.Generated.Tape

type TFState = (OpsCursor, MemCursor, Symbol, Symbol, BracketLUT)

type family InitTF (ops :: Symbol) (i :: Symbol) :: TFState where
  InitTF ops i = '(MkOpsCursor ops, MkMemCursor Tape, i, "", MkLUT ops)

type RunTF ops input = InterpNext' (InitTF ops input)

type family InterpNext (state :: TFState) :: Symbol where
  InterpNext '( '(_, _, _, '[]), _, _, o, _) = Reverse o
  InterpNext '(ops, mem, i, o, lut) = InterpNext' '(SlideR ops 1, mem, i, o, lut)

type family InterpNext' (state :: TFState) :: Symbol where
  InterpNext' '(ops, mem, i, o, lut) = InterpNext'' (GetCursor ops) '(ops, mem, i, o, lut)

type family InterpNext'' (op :: Char) (state :: TFState) :: Symbol where
  InterpNext'' '>' '(ops, mem, i, o, lut) = InterpNext '(ops, SlideR mem 1, i, o, lut)
  InterpNext'' '<' '(ops, mem, i, o, lut) = InterpNext '(ops, SlideL mem 1, i, o, lut)

  InterpNext'' '+' '(ops, mem, i, o, lut) = InterpNext '(ops, IncMem mem, i, o, lut)
  InterpNext'' '-' '(ops, mem, i, o, lut) = InterpNext '(ops, DecMem mem, i, o, lut)

  InterpNext'' '.' '(ops, mem, i, o, lut) = InterpNext '(ops, mem, i, ConsSymbol (NatToChar (GetCursor mem)) o, lut)
  InterpNext'' ',' '(ops, mem, i, o, lut) = StoreInput (UnconsSymbol i) '(ops, mem, i, o, lut)

  InterpNext'' '[' '(ops, mem, i, o, lut) = OnOpen  (CmpNat (GetCursor mem) 0) '(ops, mem, i, o, lut)
  InterpNext'' ']' '(ops, mem, i, o, lut) = OnClose (CmpNat (GetCursor mem) 0) '(ops, mem, i, o, lut)

  InterpNext'' _ state = InterpNext state

-- Need to check if any are significantly faster than the others somehow
-- type family IncMem (mem :: MemCursor) :: MemCursor where
--   IncMem '(ptr, before, 255, after) = '(ptr, before, 0, after) 
--   IncMem '(ptr, before, val, after) = '(ptr, before, val + 1, after) 

-- type family IncMem (mem :: MemCursor) :: MemCursor where
--   IncMem '(ptr, before, val, after) = '(ptr, before, Mod (val + 1) 256, after)   

type family IncMem (mem :: MemCursor) :: MemCursor where
  IncMem '(ptr, before, val, after) = '(ptr, before, val +~ 1, after) 
  
type family DecMem (mem :: MemCursor) :: MemCursor where
  DecMem '(ptr, before, val, after) = '(ptr, before, val -~ 1, after) 

type family StoreInput (i :: SCons) (state :: TFState) :: Symbol where
  StoreInput ('Just '(char, rest)) '(ops, mem, _, o, lut) = InterpNext '(ops, SetCursor mem (CharToNat char), rest, o, lut)
  -- StoreInput 'Nothing _ = TypeError ('Text "Ran out of input...")
  StoreInput 'Nothing '(_, _, i, o, _) = Reverse o

type family OnOpen (ord :: Ordering) (state :: TFState) where
  OnOpen 'EQ '(ops, mem, i, o, lut) = InterpNext '(SlideR ops (DistToClose lut (Idx ops)), mem, i, o, lut)
  OnOpen _ state = InterpNext state

type family OnClose (ord :: Ordering) (state :: TFState) where
  OnClose 'EQ state = InterpNext state
  OnClose _ '(ops, mem, i, o, lut) = InterpNext '(SlideL ops (DistToOpen lut (Idx ops)), mem, i, o, lut)

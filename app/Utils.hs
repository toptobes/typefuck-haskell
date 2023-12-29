{-# LANGUAGE AllowAmbiguousTypes #-}

module Utils where

import GHC.TypeLits (KnownSymbol, symbolVal)
import TF.Core (RunTF)

reifySym :: ∀ s. (KnownSymbol s) => String
reifySym = symbolVal (Proxy @s)

reifyTF :: ∀ code input. (KnownSymbol code, KnownSymbol (RunTF code input)) => String
reifyTF = symbolVal (Proxy @(RunTF code input))

runTest :: ∀ code input. (KnownSymbol code, KnownSymbol (RunTF code input)) => String -> IO ()
runTest expected = putStrLn . intercalate "\n" $ 
  [ "Running code: " <> reifySym @code
  , " - Expected: '" <> expected <> "'"
  , " - Actual: '" <> tf <> "'"
  , if tf == expected then "Success!" else "Failed..."
  ] where tf = reifyTF @code @input

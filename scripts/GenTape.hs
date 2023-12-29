module GenTape where

import Data.Text as T (intercalate)

input :: [Integer]
input = replicate 10 0

template :: Text -> Text
template tape = T.intercalate "\n"
  [ "module TF.Generated.Tape where"
  , "type Tape = '" <> tape
  ]

main :: IO ()
main = writeFileText "app/TF/Generated/Tape.hs" (template list)
  where list = show @Text $ input

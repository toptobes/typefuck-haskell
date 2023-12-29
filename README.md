# Typefuck (Haskell edition)

(inspired by [susisu's Typescript-edition typefuck](https://github.com/susisu/typefuck))

This Typefuck also runs solely in Haskell's type system, though (at least I think, as I haven't 
looked deeply into their code) it operates a bit differently than susisu's.

## (Roughly) How it works

The state of the program is managed by three main types:

```hs
{- | An efficient way to move forwards and backwards through some list
   (N.B. index == length preceeding) -}
type Cursor a = 
  ( Nat -- ^ The index of the currently selected thing
  , [a] -- ^ The preceeding things
  ,  a  -- ^ The thing the cursor is currently pointing at
  , [a] -- ^ The succeeding things
  )

{- | An association list to hold the index of each open bracket and its closing counterpart -}
type BracketLUT = [(Nat, Nat)]

{- | The state of the brainfuck engine
   (N.B. I also want to change the memory to be a 'Cursor Nat') -}
type TFState = 
  ( Cursor Char -- ^ The cursor for moving through the code
  , [Nat]       -- ^ The memory tape
  ,  Nat        -- ^ The pointer to the currently selected cell
  , Symbol      -- ^ The input to the program
  , Symbol      -- ^ The output for the program
  , BracketLUT  -- ^ The aforementioned LUT for the []s
  )
```

and the rest is basically mutually-recursive type-family spam, which you can read
through as you please.

The tape is (hack-ily) generated through an external haskell script which creates
the file which contains the tape (more on that later).

I also had to manually implement versions of + and - that would overflow/underflow
to emulate ubyte-sized cells:

```hs
type family (a :: Nat) -~ (b :: Nat) :: Nat where
  a -~ b = UnderflowingSub' (CmpNat a b) a b

type family UnderflowingSub' (ord :: Ordering) (a :: Nat) (b :: Nat) where
  UnderflowingSub' 'LT a b = 256 - (b - a)
  UnderflowingSub'  _  a b = a - b

type family (a :: Nat) +~ (b :: Nat) :: Nat where
  a +~ b = Mod (a + b) 256
```

## Using it for yourself

To run this, you need to have at the very least `cabal` installed, but it'd be in your
favor to also have `make` + `bash` (or a compatable shell) up and running. `node` or 
(what I use) `bun` are also used for tests.

### Setting up the tape

The tape should already be pregenerated with 10 0s; however, you can generate
a tape with the command `make tape TAPE="<list_gen>"` where `<list_gen>` is some
expression that generates a haskell list. Examples include
  - `TAPE="[1, 2, 3]"`
  - `TAPE="replicate 10 0"`
  - `TAPE="[2 * x | x <- [1..10], mod x 2 == 0]"`

and it generates a file like so:

```hs
module TF.Generated.Tape where
type Tape = '[0,0,0,0,0,0,0,0,0,0]
```

If, for some reason, you can't use the command line utility, you can just manually generate
the list yourself (make sure you don't forget to use the `'`!)

Disclaimer: you're free to generate long tapes, but if you go too long, you'd risk being overtaken
by the heat-death of the universe lol

### Actually running Typefuck

My recommended way you use typefuck (if you're just type-fucking around) is to use `make repl` (`cabal repl app`),
`import TF.Core`, and `:k! RunTF "<code>" "<input>"`:

```hs
me@t:~/projects/typefuck-haskell$ make repl
<...>
λ> import TF.Core
λ> :k! RunTF ",." "a"
RunTF ",." "a" :: ghc-prim:GHC.Types.Symbol
= "a"
λ> :k! RunTF "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+." ""
<...>
= "Hello World!"
λ>
```

You can also use `reifySF` from `Utils` to create a concrete string, but that takes
*much* longer to generate any non-trivial programs

```hs
λ> symbolVal $ Proxy @(RunTF ",." "a")
"a"
λ> reifyTF @",." @"a"
"a"
```

### Running the tests

You can run the tests with `make tests` w/ bun/node (or manually executing `spec/run-tests.mjs`).
You can add your own tests to `spec/tests.json` as you please.

## Disclaimer

I basically wrote this in a night or two so there may be some minor bugs and things that could be more efficient,
but I'm looking to fix them as I get time and motivation. Feel free to contribute!

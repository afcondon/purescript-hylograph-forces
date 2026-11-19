-- | d3-force's random source: the linear congruential generator of `lcg.js`
-- | and the `jiggle` that separates coincident nodes.
-- |
-- | Ported from d3-force 3.0.0 (ISC, Copyright 2010-2021 Mike Bostock); see
-- | LICENSE.
-- |
-- | The generator is a pure step on a `Seed`, so the tree walks can thread it
-- | as a loop variable; a `Source` is the same seed in a `Ref`, shared by the
-- | forces of one simulation the way d3 shares one `random` function.
module Hylograph.Forces.Random
  ( Seed
  , Source
  , lcg
  , initialSeed
  , advance
  , jiggle
  , draw
  ) where

import Prelude

import Data.Number (remainder)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- | The generator's state: an integer in [0, 2^32), held as a `Number`
-- | because the multiplication needs 53 bits.
newtype Seed = Seed Number

-- | A seed shared between forces.
type Source = Ref Seed

multiplier :: Number
multiplier = 1664525.0

increment :: Number
increment = 1013904223.0

modulus :: Number
modulus = 4294967296.0

-- | d3 starts every generator at 1.
initialSeed :: Seed
initialSeed = Seed 1.0

-- | d3's `lcg()`: a fresh source.
lcg :: Effect Source
lcg = Ref.new initialSeed

-- | One step: `s = (a * s + c) % m`.
advance :: Seed -> Seed
advance (Seed state) = Seed (remainder (multiplier * state + increment) modulus)

-- | d3's `jiggle(random)`, read from a seed that has just been advanced:
-- | `(s / m - 0.5) * 1e-6`.
jiggle :: Seed -> Number
jiggle (Seed state) = (state / modulus - 0.5) * 1.0e-6

-- | Advance a shared source and return its jiggle.
draw :: Source -> Effect Number
draw source = do
  seed <- Ref.read source
  let next = advance seed
  Ref.write next source
  pure (jiggle next)

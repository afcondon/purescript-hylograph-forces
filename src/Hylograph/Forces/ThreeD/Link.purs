-- | The link force in three dimensions: each link is a spring that pulls its
-- | two nodes towards a set distance apart.
-- |
-- | Ported from d3-force-3d 3.0.6, `link.js` (MIT, Copyright 2017 Vasco
-- | Asturiano; itself from d3-force, ISC, Mike Bostock); see LICENSE. It is
-- | d3-force's `link.js` with a third coordinate and nothing else.
-- |
-- | The joining of links to nodes (`Hylograph.Forces.Linkage`: degrees,
-- | bias, by place or by id) has no dimension and is the two-dimensional
-- | force's.
module Hylograph.Forces.ThreeD.Link
  ( Config
  , Strength
  , strengthFromDegree
  , strengthPerLink
  , defaultConfig
  , Link
  , initialize
  , initializeById
  , apply
  , module Shared
  ) where

import Prelude

import Data.Either (Either)
import Data.Number (sqrt)
import Effect (Effect, forE, foreachE)
import Effect.Uncurried (EffectFn5, mkEffectFn5, runEffectFn4, runEffectFn5)
import Hylograph.Forces.Linkage (Endpoints, Identified, Joined, joinByPlace, joinById)
import Hylograph.Forces.Linkage (Endpoints, Identified) as Shared
import Hylograph.Forces.Random as Random
import Hylograph.Forces.ThreeD.Node (Node, writeVelocity)

-- | How stiff each spring is.
data Strength link
  = FromDegree
  | PerLink (link -> Number)

-- | d3's default: `1 / min(degree of source, degree of target)`.
strengthFromDegree :: forall link. Strength link
strengthFromDegree = FromDegree

-- | A strength asked of each link when the force is initialized.
strengthPerLink :: forall link. (link -> Number) -> Strength link
strengthPerLink = PerLink

-- | `forceLink` settings. `distance` is asked once per link.
type Config link =
  { distance :: link -> Number
  , strength :: Strength link
  , iterations :: Int
  }

-- | d3's defaults: distance 30, strength from degree, one iteration.
defaultConfig :: forall link. Config link
defaultConfig =
  { distance: \_ -> 30.0
  , strength: FromDegree
  , iterations: 1
  }

-- | A link ready to apply. `bias` is the share of the correction that the
-- | target takes.
type Spring row =
  { source :: Node row
  , target :: Node row
  , distance :: Number
  , strength :: Number
  , bias :: Number
  }

-- | An initialized link force.
newtype Link row = Link
  { springs :: Array (Spring row)
  , iterations :: Int
  , random :: Random.Source
  }

-- | `force.initialize(nodes, random, 3)` together with `force.links(links)`,
-- | for links that name their nodes by place in the node array. A link
-- | naming a place outside the array is dropped.
initialize
  :: forall row linkRow
   . Config (Endpoints linkRow)
  -> Random.Source
  -> Array (Node row)
  -> Array (Endpoints linkRow)
  -> Link row
initialize config random nodes links = springs config random (joinByPlace nodes links)

-- | The same for links that name their nodes by id; a link naming an id
-- | that no node has is d3's error, `node not found: <id>`.
initializeById
  :: forall id row linkRow
   . Ord id
  => Show id
  => (Node row -> id)
  -> Config (Identified id linkRow)
  -> Random.Source
  -> Array (Node row)
  -> Array (Identified id linkRow)
  -> Either String (Link row)
initializeById identify config random nodes links = map (springs config random) (joinById identify nodes links)

-- | `Joined` is written for the two-dimensional node; a three-dimensional
-- | node is one with `z` and `vz` besides, so the same join serves.
springs
  :: forall row link
   . Config link
  -> Random.Source
  -> Array (Joined (z :: Number, vz :: Number | row) link)
  -> Link row
springs config random joined = Link
  { springs: map spring joined
  , iterations: config.iterations
  , random
  }
  where
  spring end =
    { source: end.source
    , target: end.target
    , distance: config.distance end.link
    , strength: case config.strength of
        FromDegree -> 1.0 / min end.sourceDegree end.targetDegree
        PerLink strength -> strength end.link
    , bias: end.bias
    }

-- | d3-force-3d's `force(alpha)`. Links are taken in order and each sees
-- | what the ones before it did.
-- |
-- | ```
-- | x = target.x + target.vx - source.x - source.vx || jiggle(random);
-- | y = target.y + target.vy - source.y - source.vy || jiggle(random);
-- | z = target.z + target.vz - source.z - source.vz || jiggle(random);
-- | ```
apply :: forall row. Link row -> Number -> Effect Unit
apply (Link force) alpha =
  forE 0 force.iterations \_ ->
    foreachE force.springs \spring ->
      let
        x = spring.target.x + spring.target.vx - spring.source.x - spring.source.vx
        y = spring.target.y + spring.target.vy - spring.source.y - spring.source.vy
        z = spring.target.z + spring.target.vz - spring.source.z - spring.source.vz
      in
        if (x < 0.0 || x > 0.0) && (y < 0.0 || y > 0.0) && (z < 0.0 || z > 0.0) then runEffectFn5 pull spring alpha x y z
        else do
          apartX <- orJiggle force.random x
          apartY <- orJiggle force.random y
          apartZ <- orJiggle force.random z
          runEffectFn5 pull spring alpha apartX apartY apartZ

-- | The body of the loop, given the separation of the link's two nodes:
-- |
-- | ```
-- | l = Math.sqrt(x * x + y * y + z * z);
-- | l = (l - distances[i]) / l * alpha * strengths[i];
-- | x *= l, y *= l, z *= l;
-- | target.vx -= x * (b = bias[i]); target.vy -= y * b; target.vz -= z * b;
-- | source.vx += x * (b = 1 - b); source.vy += y * b; source.vz += z * b;
-- | ```
-- |
-- | The target is written before the source's velocity is read, so a link
-- | from a node to itself comes out as it does in d3.
pull :: forall row. EffectFn5 (Spring row) Number Number Number Number Unit
pull = mkEffectFn5 \spring alpha x y z -> do
  let
    separation = sqrt (x * x + y * y + z * z)
    scale = (separation - spring.distance) / separation * alpha * spring.strength
    pullX = x * scale
    pullY = y * scale
    pullZ = z * scale
    share = 1.0 - spring.bias
  runEffectFn4 writeVelocity spring.target
    (spring.target.vx - pullX * spring.bias)
    (spring.target.vy - pullY * spring.bias)
    (spring.target.vz - pullZ * spring.bias)
  runEffectFn4 writeVelocity spring.source
    (spring.source.vx + pullX * share)
    (spring.source.vy + pullY * share)
    (spring.source.vz + pullZ * share)

-- | `value || jiggle(random)`: the value, unless it is zero or `NaN`.
orJiggle :: Random.Source -> Number -> Effect Number
orJiggle random value =
  if value < 0.0 || value > 0.0 then pure value
  else Random.draw random

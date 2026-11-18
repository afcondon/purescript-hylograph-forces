-- | The link force: each link is a spring that pulls its two nodes towards a
-- | set distance apart.
-- |
-- | Ported from d3-force 3.0.0, `link.js` (ISC, Copyright 2010-2021 Mike
-- | Bostock); see LICENSE.
-- |
-- | A link names its nodes by their place in the node array (`initialize`)
-- | or by an id read off each node (`initializeById`, which is d3's
-- | `force.id(...)` and gives d3's error for a link to a node that is not
-- | there).
module Hylograph.Forces.Pure.Link
  ( Config
  , Strength
  , strengthFromDegree
  , strengthPerLink
  , defaultConfig
  , Link
  , initialize
  , initializeById
  , apply
  , module Linkage
  ) where

import Prelude

import Data.Either (Either)
import Data.Number (sqrt)
import Effect (Effect, forE, foreachE)
import Effect.Uncurried (EffectFn4, mkEffectFn4, runEffectFn3, runEffectFn4)
import Hylograph.Forces.Pure.Linkage (Endpoints, Identified) as Linkage
import Hylograph.Forces.Pure.Linkage (Joined, joinByPlace, joinById)
import Hylograph.Forces.Pure.Node (Node, writeVelocity)
import Hylograph.Forces.Pure.Random as Random

-- | How stiff each spring is.
data Strength link
  = FromDegree
  | PerLink (link -> Number)

-- | d3's default: `1 / min(degree of source, degree of target)`, so that
-- | links to heavily connected nodes are weaker.
strengthFromDegree :: forall link. Strength link
strengthFromDegree = FromDegree

-- | A strength asked of each link when the force is initialized.
strengthPerLink :: forall link. (link -> Number) -> Strength link
strengthPerLink = PerLink

-- | d3's `forceLink` settings. `distance` is asked once per link.
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
-- | target takes: d3's `count[source] / (count[source] + count[target])`.
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

-- | d3's `force.initialize(nodes, random)` together with `force.links(links)`,
-- | for links that name their nodes by place in the node array. A link
-- | naming a place outside the array is dropped.
initialize
  :: forall row linkRow
   . Config (Linkage.Endpoints linkRow)
  -> Random.Source
  -> Array (Node row)
  -> Array (Linkage.Endpoints linkRow)
  -> Link row
initialize config random nodes links = springs config random (joinByPlace nodes links)

-- | The same for links that name their nodes by id, as d3's `force.id(...)`
-- | has it and as Hylograph's links do. A link naming an id that no node has
-- | is d3's error, `node not found: <id>`.
initializeById
  :: forall id row linkRow
   . Ord id
  => Show id
  => (Node row -> id)
  -> Config (Linkage.Identified id linkRow)
  -> Random.Source
  -> Array (Node row)
  -> Array (Linkage.Identified id linkRow)
  -> Either String (Link row)
initializeById identify config random nodes links = map (springs config random) (joinById identify nodes links)

springs :: forall row link. Config link -> Random.Source -> Array (Joined row link) -> Link row
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

-- | d3's `force(alpha)`: move the velocities of each link's nodes so that
-- | the link comes closer to its distance. Links are taken in order and each
-- | sees what the ones before it did.
apply :: forall row. Link row -> Number -> Effect Unit
apply (Link force) alpha =
  forE 0 force.iterations \_ ->
    foreachE force.springs \spring ->
      let
        x = spring.target.x + spring.target.vx - spring.source.x - spring.source.vx
        y = spring.target.y + spring.target.vy - spring.source.y - spring.source.vy
      in
        -- d3: `x = ... || jiggle(random)`, and the same for y. Nearly always
        -- neither is needed.
        if (x < 0.0 || x > 0.0) && (y < 0.0 || y > 0.0) then runEffectFn4 pull spring alpha x y
        else do
          apartX <- orJiggle force.random x
          apartY <- orJiggle force.random y
          runEffectFn4 pull spring alpha apartX apartY

-- | The body of d3's loop, given the separation of the link's two nodes:
-- |
-- | ```
-- | l = Math.sqrt(x * x + y * y);
-- | l = (l - distances[i]) / l * alpha * strengths[i];
-- | x *= l, y *= l;
-- | target.vx -= x * (b = bias[i]);
-- | target.vy -= y * b;
-- | source.vx += x * (b = 1 - b);
-- | source.vy += y * b;
-- | ```
-- |
-- | The target is written before the source's velocity is read, so a link
-- | from a node to itself comes out as it does in d3.
pull :: forall row. EffectFn4 (Spring row) Number Number Number Unit
pull = mkEffectFn4 \spring alpha x y -> do
  let
    separation = sqrt (x * x + y * y)
    scale = (separation - spring.distance) / separation * alpha * spring.strength
    pullX = x * scale
    pullY = y * scale
    share = 1.0 - spring.bias
  runEffectFn3 writeVelocity spring.target
    (spring.target.vx - pullX * spring.bias)
    (spring.target.vy - pullY * spring.bias)
  runEffectFn3 writeVelocity spring.source
    (spring.source.vx + pullX * share)
    (spring.source.vy + pullY * share)

-- | d3's `value || jiggle(random)`: the value, unless it is zero or `NaN`.
orJiggle :: Random.Source -> Number -> Effect Number
orJiggle random value =
  if value < 0.0 || value > 0.0 then pure value
  else Random.draw random

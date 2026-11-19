-- | The collision force: treat each node as a circle and push overlapping
-- | circles apart.
-- |
-- | Ported from d3-force 3.0.0, `collide.js` (ISC, Copyright 2010-2021 Mike
-- | Bostock); see LICENSE.
-- |
-- | The shape is the many-body force's. Each iteration builds a quadtree of
-- | where the nodes are about to be (`x + vx`), turns it into a chain of
-- | `Region`s that each know the largest radius inside them (d3's `prepare`),
-- | and walks the chain once per node. A walk is pure: it returns the node's
-- | new velocity and the new velocities of the nodes it pushed, and those
-- | are written when the walk has returned. That is the same as d3 writing
-- | them as it goes, because a walk meets each other node at most once and
-- | never reads back a velocity it has changed.
-- |
-- | Like d3, this looks only at the newest of several nodes at exactly the
-- | same place; the older ones are not collided with until something else
-- | moves them.
module Hylograph.Forces.Collide
  ( Config
  , defaultConfig
  , Collide
  , initialize
  , apply
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Function.Uncurried (Fn10, Fn3, Fn5, Fn6, mkFn10, mkFn3, mkFn5, mkFn6, runFn10, runFn3, runFn5, runFn6)
import Data.List (List(..))
import Data.Number (sqrt)
import Effect (Effect, forE, foreachE, untilE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Node (Node, writeVelocity)
import Hylograph.Forces.Quadtree (Point, Quadtree(..))
import Hylograph.Forces.Quadtree as Quadtree
import Hylograph.Forces.Random (Seed)
import Hylograph.Forces.Random as Random

-- | d3's `forceCollide` settings. `radius` is asked once per node when the
-- | force is initialized.
type Config row =
  { radius :: Node row -> Number
  , strength :: Number
  , iterations :: Int
  , include :: Node row -> Boolean
  , alphaScaled :: Boolean
  }

-- | d3's defaults: radius 1, full strength, one iteration; and the two
-- | things d3 does not have turned off.
-- |
-- | Those two are the Rust kernel's "yielding collision" (`apply_collide`
-- | in `force-kernel/src/forces.rs`), held to it by `test/differential.mjs`:
-- |
-- | - `alphaScaled`: the strength used is `strength * alpha`, so the
-- |   collision weakens as the simulation cools and gives way to forces
-- |   that hold a node at a place;
-- | - `include`: a pair is tested only if both of its nodes are included. A
-- |   node left out neither pushes nor is pushed. (It is still in the
-- |   quadtree, where it can only widen the search.)
defaultConfig :: forall row. Config row
defaultConfig =
  { radius: \_ -> 1.0
  , strength: 1.0
  , iterations: 1
  , include: \_ -> true
  , alphaScaled: false
  }

-- | A node as the force sees it: its place in the node array, its radius,
-- | and whether it collides at all.
type Body row = { index :: Int, radius :: Number, included :: Boolean, node :: Node row }

-- | An initialized collision force.
newtype Collide row = Collide
  { bodies :: Array (Body row)
  , strength :: Number
  , iterations :: Int
  , alphaScaled :: Boolean
  , random :: Random.Source
  }

-- | d3's `force.initialize(nodes, random)`. A node's index is its place in
-- | the array, which is what d3's simulation sets `node.index` to.
initialize :: forall row. Config row -> Random.Source -> Array (Node row) -> Collide row
initialize config random nodes = Collide
  { bodies: mapWithIndex (\index node -> { index, radius: config.radius node, included: config.include node, node }) nodes
  , strength: config.strength
  , iterations: config.iterations
  , alphaScaled: config.alphaScaled
  , random
  }

-- | d3's `force()`: resolve overlaps, `iterations` times over. Alpha is
-- | used only by a collision whose strength is scaled by it.
apply :: forall row. Collide row -> Number -> Effect Unit
apply (Collide force) alpha =
  forE 0 force.iterations \_ -> do
    points <- anticipate force.bodies
    let chain = regions (Quadtree.build points)
    foreachE force.bodies \body ->
      when body.included do
        seed <- Ref.read force.random
        let
          moved = runFn10 walk strength body.index body.radius
            (body.node.x + body.node.vx)
            (body.node.y + body.node.vy)
            chain
            body.node.vx
            body.node.vy
            seed
            NoPushes
        runEffectFn3 writeVelocity body.node moved.vx moved.vy
        Ref.write moved.seed force.random
        deliver moved.pushes
  where
  strength = if force.alphaScaled then force.strength * alpha else force.strength

-- | Where each node will be once its velocity is added, newest first. An
-- | effect, because the nodes are mutable: they are read when the force is
-- | applied, not when the application is described.
anticipate :: forall row. Array (Body row) -> Effect (List (Point (Body row)))
anticipate bodies = do
  points <- Ref.new Nil
  foreachE bodies \body -> do
    earlier <- Ref.read points
    Ref.write (Cons { x: body.node.x + body.node.vx, y: body.node.y + body.node.vy, item: body } earlier) points
  Ref.read points

-- | One square of the tree, ready for the walk. The fields are, in order:
-- |
-- | 1. the largest radius inside the square (d3's `quad.r`);
-- | 2. to 5. the square's corners: x0, y0, x1, y1;
-- | 6. inside: where the walk goes to open the square;
-- | 7. after: where the walk goes when the square is done;
-- | 8. the node a leaf stands for, and none for a square with quarters.
-- |
-- | As in the many-body force, they sit in the constructor rather than in a
-- | record so that the walk has one object fewer to follow a pointer to.
data Region row
  = Region Number Number Number Number Number (Region row) (Region row) (Occupant row)
  | Finished

-- | The node a leaf stands for (the newest of the nodes at that place): its
-- | index and the node.
data Occupant row
  = Unoccupied
  | Occupant Int (Node row)

-- | The whole tree as a chain of regions: d3's `visitAfter(prepare)`, and
-- | the threading that lets the walk run without a stack.
regions :: forall row. Quadtree.Planted (Body row) -> Region row
regions planted =
  runFn6 thread planted.root planted.extent.x0 planted.extent.y0 planted.extent.x1 planted.extent.y1 Finished
  where
  thread :: Fn6 (Quadtree (Body row)) Number Number Number Number (Region row) (Region row)
  thread = mkFn6 \tree x0 y0 x1 y1 after -> case tree of
    Empty -> after
    Leaf newest _ ->
      Region newest.item.radius x0 y0 x1 y1 after after (occupantOf newest.item)
    Branch topLeft topRight bottomLeft bottomRight ->
      let
        middleX = (x0 + x1) / 2.0
        middleY = (y0 + y1) / 2.0
        fourth = runFn6 thread bottomRight middleX middleY x1 y1 after
        third = runFn6 thread bottomLeft x0 middleY middleX y1 fourth
        second = runFn6 thread topRight middleX y0 x1 middleY third
        first = runFn6 thread topLeft x0 y0 middleX middleY second
        radius =
          runFn3 larger bottomRight fourth
            (runFn3 larger bottomLeft third (runFn3 larger topRight second (runFn3 larger topLeft first 0.0)))
      in
        Region radius x0 y0 x1 y1 first after Unoccupied

  -- A node left out of the collision stands in its leaf under an index no
  -- walking node's index is below, so that no walk ever tests it.
  occupantOf :: Body row -> Occupant row
  occupantOf body = Occupant (if body.included then body.index else -1) body.node

  -- d3: `if (quad[i] && quad[i].r > quad.r) quad.r = quad[i].r`
  larger :: Fn3 (Quadtree (Body row)) (Region row) Number Number
  larger = mkFn3 \quarter region radius -> case quarter of
    Empty -> radius
    _ -> case region of
      Region inner _ _ _ _ _ _ _ -> if inner > radius then inner else radius
      Finished -> radius

-- | The velocities a walk gave to the nodes it pushed.
data Pushes row
  = NoPushes
  | Push (Node row) Number Number (Pushes row)

-- | What a walk leaves behind.
type Moved row = { vx :: Number, vy :: Number, seed :: Seed, pushes :: Pushes row }

-- | d3's `tree.visit(apply)` for one node. Its arguments are the force's
-- | strength, the node's index, its radius and its anticipated position, the
-- | chain, and the velocity, seed and pushes to start from; they are taken
-- | all at once (`Fn10`) so that calling it does not build ten closures per
-- | node.
-- |
-- | It goes from one overlapping leaf to the next. Finding the next one is
-- | `seek`, a loop with the chain as its only variable; nearly all of the
-- | walk is spent there.
walk :: forall row. Fn10 Number Int Number Number Number (Region row) Number Number Seed (Pushes row) (Moved row)
walk = mkFn10 \strength index radius nodeX nodeY chain startVx startVy startSeed startPushes ->
  let
    go start vx vy seed pushes = case runFn5 seek index radius nodeX nodeY start of
      Region inner _ _ _ _ _ after (Occupant _ node) ->
        let
          x = nodeX - node.x - node.vx
          y = nodeY - node.y - node.vy
          bounce = rebound strength radius inner x y (x * x + y * y) seed
        in
          go after
            (vx + bounce.x * bounce.share)
            (vy + bounce.y * bounce.share)
            bounce.seed
            ( Push node
                (node.vx - bounce.x * (1.0 - bounce.share))
                (node.vy - bounce.y * (1.0 - bounce.share))
                pushes
            )
      _ -> { vx, vy, seed, pushes }
  in
    go chain startVx startVy startSeed startPushes

-- | Along the chain from `start` to the next leaf whose node overlaps the
-- | walking node, or to the end: d3's `apply` up to the point where it finds
-- | `l < r * r`.
-- |
-- | ```
-- | var data = quad.data, rj = quad.r, r = ri + rj;
-- | if (data) {
-- |   if (data.index > node.index) {
-- |     var x = xi - data.x - data.vx, y = yi - data.y - data.vy, l = x * x + y * y;
-- |     if (l < r * r) { ... }
-- |   }
-- |   return;
-- | }
-- | return x0 > xi + r || x1 < xi - r || y0 > yi + r || y1 < yi - r;
-- | ```
seek :: forall row. Fn5 Int Number Number Number (Region row) (Region row)
seek = mkFn5 \index radius nodeX nodeY start ->
  let
    go region = case region of
      Region inner x0 y0 x1 y1 inside after occupant ->
        let
          reach = radius + inner
        in
          case occupant of
            Unoccupied ->
              if x0 > nodeX + reach || x1 < nodeX - reach || y0 > nodeY + reach || y1 < nodeY - reach then go after
              else go inside
            Occupant other node ->
              if other > index then
                let
                  x = nodeX - node.x - node.vx
                  y = nodeY - node.y - node.vy
                in
                  if x * x + y * y < reach * reach then region
                  else go after
              else go after
      Finished -> Finished
  in
    go start

-- | The correction for two overlapping circles: a displacement and the share
-- | of it that the walking node takes.
type Rebound = { x :: Number, y :: Number, share :: Number, seed :: Seed }

-- | d3:
-- |
-- | ```
-- | if (x === 0) x = jiggle(random), l += x * x;
-- | if (y === 0) y = jiggle(random), l += y * y;
-- | l = (r - (l = Math.sqrt(l))) / l * strength;
-- | node.vx += (x *= l) * (r = (rj *= rj) / (ri2 + rj));
-- | node.vy += (y *= l) * r;
-- | data.vx -= x * (r = 1 - r);
-- | data.vy -= y * r;
-- | ```
rebound :: Number -> Number -> Number -> Number -> Number -> Number -> Seed -> Rebound
rebound strength radius otherRadius x y distance2 seed =
  let
    seedX = if x == 0.0 then Random.advance seed else seed
    apartX = if x == 0.0 then Random.jiggle seedX else x
    lengthX = if x == 0.0 then distance2 + apartX * apartX else distance2
    seedY = if y == 0.0 then Random.advance seedX else seedX
    apartY = if y == 0.0 then Random.jiggle seedY else y
    lengthY = if y == 0.0 then lengthX + apartY * apartY else lengthX
    distance = sqrt lengthY
    scale = (radius + otherRadius - distance) / distance * strength
    otherRadius2 = otherRadius * otherRadius
  in
    { x: apartX * scale
    , y: apartY * scale
    , share: otherRadius2 / (radius * radius + otherRadius2)
    , seed: seedY
    }

-- | Write the velocities a walk gave to the nodes it pushed.
deliver :: forall row. Pushes row -> Effect Unit
deliver pushes = case pushes of
  NoPushes -> pure unit
  _ -> do
    waiting <- Ref.new pushes
    untilE do
      remaining <- Ref.read waiting
      case remaining of
        NoPushes -> pure true
        Push node vx vy rest -> do
          runEffectFn3 writeVelocity node vx vy
          Ref.write rest waiting
          pure false

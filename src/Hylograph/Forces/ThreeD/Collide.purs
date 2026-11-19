-- | The collision force in three dimensions: treat each node as a sphere and
-- | push overlapping spheres apart.
-- |
-- | Ported from d3-force-3d 3.0.6, `collide.js` (MIT, Copyright 2017 Vasco
-- | Asturiano; itself from d3-force, ISC, Mike Bostock); see LICENSE. It is
-- | d3-force's `collide.js` with a third coordinate, over d3-octree, and the
-- | shape is `Hylograph.Forces.Collide`'s: each iteration builds the octree
-- | of where the nodes are about to be (`x + vx`), turns it into a chain of
-- | `Region`s that know the largest radius inside them, and walks the chain
-- | once per node. A walk returns the node's new velocity and the new
-- | velocities of the nodes it pushed, which are written when it returns.
-- |
-- | Like d3, this looks only at the newest of several nodes at exactly the
-- | same place.
module Hylograph.Forces.ThreeD.Collide
  ( Config
  , defaultConfig
  , Collide
  , initialize
  , apply
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Function.Uncurried (Fn3, Fn6, Fn8, Fn9, mkFn3, mkFn6, mkFn8, mkFn9, runFn3, runFn6, runFn8, runFn9)
import Data.List (List(..))
import Data.Number (sqrt)
import Effect (Effect, forE, foreachE, untilE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn4)
import Hylograph.Forces.Random (Seed)
import Hylograph.Forces.Random as Random
import Hylograph.Forces.ThreeD.Node (Node, writeVelocity)
import Hylograph.Forces.ThreeD.Octree (Octree(..), Point)
import Hylograph.Forces.ThreeD.Octree as Octree

-- | `forceCollide` settings. `radius` is asked once per node when the force
-- | is initialized.
type Config row =
  { radius :: Node row -> Number
  , strength :: Number
  , iterations :: Int
  }

-- | d3's defaults: radius 1, full strength, one iteration.
defaultConfig :: forall row. Config row
defaultConfig = { radius: \_ -> 1.0, strength: 1.0, iterations: 1 }

-- | A node as the force sees it: its place in the node array and its
-- | radius.
type Body row = { index :: Int, radius :: Number, node :: Node row }

-- | An initialized collision force.
newtype Collide row = Collide
  { bodies :: Array (Body row)
  , strength :: Number
  , iterations :: Int
  , random :: Random.Source
  }

-- | `force.initialize(nodes, random, 3)`.
initialize :: forall row. Config row -> Random.Source -> Array (Node row) -> Collide row
initialize config random nodes = Collide
  { bodies: mapWithIndex (\index node -> { index, radius: config.radius node, node }) nodes
  , strength: config.strength
  , iterations: config.iterations
  , random
  }

-- | `force()`: resolve overlaps, `iterations` times over.
apply :: forall row. Collide row -> Effect Unit
apply (Collide force) =
  forE 0 force.iterations \_ -> do
    points <- anticipate force.bodies
    let chain = regions (Octree.build points)
    foreachE force.bodies \body -> do
      seed <- Ref.read force.random
      let
        walker = { strength: force.strength, index: body.index, radius: body.radius }
        moved = runFn9 walk walker
          (body.node.x + body.node.vx)
          (body.node.y + body.node.vy)
          (body.node.z + body.node.vz)
          chain
          body.node.vx
          body.node.vy
          body.node.vz
          seed
      runEffectFn4 writeVelocity body.node moved.vx moved.vy moved.vz
      Ref.write moved.seed force.random
      deliver moved.pushes

-- | Where each node will be once its velocity is added, newest first, read
-- | when the force is applied.
anticipate :: forall row. Array (Body row) -> Effect (List (Point (Body row)))
anticipate bodies = do
  points <- Ref.new Nil
  foreachE bodies \body -> do
    earlier <- Ref.read points
    Ref.write
      (Cons { x: body.node.x + body.node.vx, y: body.node.y + body.node.vy, z: body.node.z + body.node.vz, item: body } earlier)
      points
  Ref.read points

-- | One cube of the tree, ready for the walk: the largest radius inside it,
-- | its corners (x0, y0, z0, x1, y1, z1), where to go to open it, where to
-- | go when it is done, and the node a leaf stands for.
data Region row
  = Region Number Number Number Number Number Number Number (Region row) (Region row) (Occupant row)
  | Finished

-- | The node a leaf stands for (the newest at that place): index and node.
data Occupant row
  = Unoccupied
  | Occupant Int (Node row)

-- | The corners of a cube.
type Corners = { x0 :: Number, y0 :: Number, z0 :: Number, x1 :: Number, y1 :: Number, z1 :: Number }

-- | The whole tree as a chain of regions: `visitAfter(prepare)`, and the
-- | threading that lets the walk run without a stack.
regions :: forall row. Octree.Planted (Body row) -> Region row
regions planted = runFn3 thread planted.root planted.extent Finished
  where
  thread :: Fn3 (Octree (Body row)) Corners (Region row) (Region row)
  thread = mkFn3 \tree cube after -> case tree of
    Empty -> after
    Leaf newest _ ->
      Region newest.item.radius cube.x0 cube.y0 cube.z0 cube.x1 cube.y1 cube.z1 after after
        (Occupant newest.item.index newest.item.node)
    Branch o0 o1 o2 o3 o4 o5 o6 o7 ->
      let
        middleX = (cube.x0 + cube.x1) / 2.0
        middleY = (cube.y0 + cube.y1) / 2.0
        middleZ = (cube.z0 + cube.z1) / 2.0
        part right bottom deep =
          { x0: if right then middleX else cube.x0
          , y0: if bottom then middleY else cube.y0
          , z0: if deep then middleZ else cube.z0
          , x1: if right then cube.x1 else middleX
          , y1: if bottom then cube.y1 else middleY
          , z1: if deep then cube.z1 else middleZ
          }
        r7 = runFn3 thread o7 (part true true true) after
        r6 = runFn3 thread o6 (part false true true) r7
        r5 = runFn3 thread o5 (part true false true) r6
        r4 = runFn3 thread o4 (part false false true) r5
        r3 = runFn3 thread o3 (part true true false) r4
        r2 = runFn3 thread o2 (part false true false) r3
        r1 = runFn3 thread o1 (part true false false) r2
        r0 = runFn3 thread o0 (part false false false) r1
        radius =
          runFn3 larger o7 r7
            ( runFn3 larger o6 r6
                ( runFn3 larger o5 r5
                    ( runFn3 larger o4 r4
                        (runFn3 larger o3 r3 (runFn3 larger o2 r2 (runFn3 larger o1 r1 (runFn3 larger o0 r0 0.0))))
                    )
                )
            )
      in
        Region radius cube.x0 cube.y0 cube.z0 cube.x1 cube.y1 cube.z1 r0 after Unoccupied

  -- `if (treeNode[i] && treeNode[i].r > treeNode.r) treeNode.r = treeNode[i].r;`
  larger :: Fn3 (Octree (Body row)) (Region row) Number Number
  larger = mkFn3 \octant region radius -> case octant of
    Empty -> radius
    _ -> case region of
      Region inner _ _ _ _ _ _ _ _ _ -> if inner > radius then inner else radius
      Finished -> radius

-- | The velocities a walk gave to the nodes it pushed.
data Pushes row
  = NoPushes
  | Push (Node row) Number Number Number (Pushes row)

-- | What a walk leaves behind.
type Moved row = { vx :: Number, vy :: Number, vz :: Number, seed :: Seed, pushes :: Pushes row }

-- | The walking node: the force's strength, its index and its radius.
type Walker = { strength :: Number, index :: Int, radius :: Number }

-- | `tree.visit(apply)` for one node, from one overlapping leaf to the next
-- | (`seek` finds the next one).
walk :: forall row. Fn9 Walker Number Number Number (Region row) Number Number Number Seed (Moved row)
walk = mkFn9 \walker nodeX nodeY nodeZ chain startVx startVy startVz startSeed ->
  let
    strength = walker.strength
    index = walker.index
    radius = walker.radius

    go start vx vy vz seed pushes = case runFn6 seek index radius nodeX nodeY nodeZ start of
      Region inner _ _ _ _ _ _ _ after (Occupant _ node) ->
        let
          x = nodeX - node.x - node.vx
          y = nodeY - node.y - node.vy
          z = nodeZ - node.z - node.vz
          bounce = runFn8 rebound strength radius inner x y z (x * x + y * y + z * z) seed
        in
          go after
            (vx + bounce.x * bounce.share)
            (vy + bounce.y * bounce.share)
            (vz + bounce.z * bounce.share)
            bounce.seed
            ( Push node
                (node.vx - bounce.x * (1.0 - bounce.share))
                (node.vy - bounce.y * (1.0 - bounce.share))
                (node.vz - bounce.z * (1.0 - bounce.share))
                pushes
            )
      _ -> { vx, vy, vz, seed, pushes }
  in
    go chain startVx startVy startVz startSeed NoPushes

-- | Along the chain from `start` to the next leaf whose node overlaps the
-- | walking node, or to the end:
-- |
-- | ```
-- | var data = treeNode.data, rj = treeNode.r, r = ri + rj;
-- | if (data) {
-- |   if (data.index > node.index) {
-- |     var x = xi - data.x - data.vx, y = yi - data.y - data.vy, z = zi - data.z - data.vz,
-- |         l = x * x + y * y + z * z;
-- |     if (l < r * r) { ... }
-- |   }
-- |   return;
-- | }
-- | return x0 > xi + r || x1 < xi - r || y0 > yi + r || y1 < yi - r || z0 > zi + r || z1 < zi - r;
-- | ```
seek :: forall row. Fn6 Int Number Number Number Number (Region row) (Region row)
seek = mkFn6 \index radius nodeX nodeY nodeZ start ->
  let
    go region = case region of
      Region inner x0 y0 z0 x1 y1 z1 inside after occupant ->
        let
          reach = radius + inner
        in
          case occupant of
            Unoccupied ->
              if
                x0 > nodeX + reach || x1 < nodeX - reach || y0 > nodeY + reach || y1 < nodeY - reach
                  || z0 > nodeZ + reach
                  || z1 < nodeZ - reach then go after
              else go inside
            Occupant other node ->
              if other > index then
                let
                  x = nodeX - node.x - node.vx
                  y = nodeY - node.y - node.vy
                  z = nodeZ - node.z - node.vz
                in
                  if x * x + y * y + z * z < reach * reach then region
                  else go after
              else go after
      Finished -> Finished
  in
    go start

-- | The correction for two overlapping spheres: a displacement and the
-- | share of it that the walking node takes.
type Rebound = { x :: Number, y :: Number, z :: Number, share :: Number, seed :: Seed }

-- | ```
-- | if (x === 0) x = jiggle(random), l += x * x;
-- | if (y === 0) y = jiggle(random), l += y * y;
-- | if (z === 0) z = jiggle(random), l += z * z;
-- | l = (r - (l = Math.sqrt(l))) / l * strength;
-- | node.vx += (x *= l) * (r = (rj *= rj) / (ri2 + rj));
-- | node.vy += (y *= l) * r;
-- | node.vz += (z *= l) * r;
-- | data.vx -= x * (r = 1 - r);
-- | data.vy -= y * r;
-- | data.vz -= z * r;
-- | ```
rebound :: Fn8 Number Number Number Number Number Number Number Seed Rebound
rebound = mkFn8 \strength radius otherRadius x y z distance2 seed ->
  let
    seedX = if x == 0.0 then Random.advance seed else seed
    apartX = if x == 0.0 then Random.jiggle seedX else x
    lengthX = if x == 0.0 then distance2 + apartX * apartX else distance2
    seedY = if y == 0.0 then Random.advance seedX else seedX
    apartY = if y == 0.0 then Random.jiggle seedY else y
    lengthY = if y == 0.0 then lengthX + apartY * apartY else lengthX
    seedZ = if z == 0.0 then Random.advance seedY else seedY
    apartZ = if z == 0.0 then Random.jiggle seedZ else z
    lengthZ = if z == 0.0 then lengthY + apartZ * apartZ else lengthY
    distance = sqrt lengthZ
    scale = (radius + otherRadius - distance) / distance * strength
    otherRadius2 = otherRadius * otherRadius
  in
    { x: apartX * scale
    , y: apartY * scale
    , z: apartZ * scale
    , share: otherRadius2 / (radius * radius + otherRadius2)
    , seed: seedZ
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
        Push node vx vy vz rest -> do
          runEffectFn4 writeVelocity node vx vy vz
          Ref.write rest waiting
          pure false

-- | The many-body force in three dimensions: every node attracts or repels
-- | every other, with the far field approximated by Barnes-Hut over an
-- | octree.
-- |
-- | Ported from d3-force-3d 3.0.6, `manyBody.js` (MIT, Copyright 2017 Vasco
-- | Asturiano; itself from d3-force, ISC, Mike Bostock); see LICENSE. The
-- | shape is `Hylograph.Forces.ManyBody`'s: build the tree, turn it into a
-- | chain of clusters in d3's visiting order, walk the chain once per node.
-- |
-- | Where d3-force-3d is more than d3-force with a third coordinate, in
-- | `accumulate`:
-- |
-- | - the summed strength of a cube with octants is scaled by
-- |   `Math.sqrt(4 / 8)` ("according to number of dimensions"), at every
-- |   level of the tree, so a far cluster pulls with a fraction of its
-- |   nodes' strength that shrinks with its depth from the leaves; and
-- | - a child's centre is read as `q.x || 0` (zero where it is `NaN`).
module Hylograph.Forces.ThreeD.ManyBody
  ( Config
  , defaultConfig
  , ManyBody
  , initialize
  , apply
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Function.Uncurried (Fn10, Fn3, Fn4, Fn6, Fn9, mkFn10, mkFn3, mkFn4, mkFn6, mkFn9, runFn10, runFn3, runFn4, runFn6, runFn9)
import Data.List (List(..), reverse)
import Data.Number (abs, infinity, sqrt)
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn4)
import Hylograph.Forces.Random (Seed)
import Hylograph.Forces.Random as Random
import Hylograph.Forces.ThreeD.Node (Node, writeVelocity)
import Hylograph.Forces.ThreeD.Octree (Octree(..), Point)
import Hylograph.Forces.ThreeD.Octree as Octree

-- | `forceManyBody` settings. `strength` is asked once per node when the
-- | force is initialized; negative repels.
type Config row =
  { strength :: Node row -> Number
  , theta :: Number
  , distanceMin :: Number
  , distanceMax :: Number
  }

-- | d3's defaults: strength -30, theta 0.9, distances from 1 to infinity.
defaultConfig :: forall row. Config row
defaultConfig = { strength: \_ -> -30.0, theta: 0.9, distanceMin: 1.0, distanceMax: infinity }

-- | A node as the force sees it: its place in the node array and its
-- | strength.
type Body row = { index :: Int, strength :: Number, node :: Node row }

-- | The squared distances the walk compares against.
type Limits = { theta2 :: Number, distanceMin2 :: Number, distanceMax2 :: Number }

-- | What a walk is for: alpha, the limits, and the index of the walking
-- | node.
type Walker = { alpha :: Number, distanceMin2 :: Number, distanceMax2 :: Number, index :: Int }

-- | An initialized many-body force.
newtype ManyBody row = ManyBody
  { bodies :: Array (Body row)
  , limits :: Limits
  , random :: Random.Source
  }

-- | `force.initialize(nodes, random, 3)`. A node's index is its place in the
-- | array, which is what the simulation sets `node.index` to.
initialize :: forall row. Config row -> Random.Source -> Array (Node row) -> ManyBody row
initialize config random nodes = ManyBody
  { bodies: mapWithIndex (\index node -> { index, strength: config.strength node, node }) nodes
  , limits:
      { theta2: config.theta * config.theta
      , distanceMin2: config.distanceMin * config.distanceMin
      , distanceMax2: config.distanceMax * config.distanceMax
      }
  , random
  }

-- | `force(alpha)`: add the many-body acceleration to every node's velocity.
apply :: forall row. ManyBody row -> Number -> Effect Unit
apply (ManyBody force) alpha = do
  points <- snapshot force.bodies
  let chain = clusters force.limits.theta2 (Octree.build points)
  foreachE force.bodies \body -> do
    seed <- Ref.read force.random
    let
      walker = { alpha, distanceMin2: force.limits.distanceMin2, distanceMax2: force.limits.distanceMax2, index: body.index }
      moved = runFn9 walk walker body.node.x body.node.y body.node.z chain body.node.vx body.node.vy body.node.vz seed
    runEffectFn4 writeVelocity body.node moved.vx moved.vy moved.vz
    Ref.write moved.seed force.random

-- | Where the nodes are now, newest first, read when the force is applied.
snapshot :: forall row. Array (Body row) -> Effect (List (Point (Body row)))
snapshot bodies = do
  points <- Ref.new Nil
  foreachE bodies \body -> do
    earlier <- Ref.read points
    Ref.write (Cons { x: body.node.x, y: body.node.y, z: body.node.z, item: body } earlier) points
  Ref.read points

-- | One cube of the tree, ready for the walk: the summed (and scaled)
-- | strength, the centre of that strength in x, y and z, the reach
-- | (`w * w / theta2`), where to go to open the cube, where to go when it is
-- | done, and the nodes of a leaf.
data Cluster
  = Cluster Number Number Number Number Number Cluster Cluster Members
  | Finished

-- | The nodes at one location, newest first: index, strength, older nodes.
data Members
  = NoMembers
  | Member Int Number Members

-- | The running sums of `accumulate` over a cube's octants: strength,
-- | weight, and the weighted x, y and z.
data Sums = Sums Number Number Number Number Number

-- | d3-force-3d's scale for a cube with octants: `Math.sqrt(4 / numChildren)`,
-- | where an octree's inner node has eight.
octantScale :: Number
octantScale = sqrt (4.0 / 8.0)

-- | The whole tree as a chain of clusters.
clusters :: forall row. Number -> Octree.Planted (Body row) -> Cluster
clusters theta2 planted = runFn4 thread planted.root planted.extent.x0 planted.extent.x1 Finished
  where
  -- The chain for one cube, which runs on into `after`. Octants are threaded
  -- last to first so that each knows what follows it, and summed first to
  -- last, the order d3-force-3d adds them in.
  thread :: Fn4 (Octree (Body row)) Number Number Cluster Cluster
  thread = mkFn4 \tree x0 x1 after -> case tree of
    Empty -> after
    Leaf newest older ->
      let
        nodes = membersOf (reverse (Cons newest older)) NoMembers
      in
        Cluster (total nodes 0.0) newest.x newest.y newest.z (reach x0 x1) after after nodes
    Branch o0 o1 o2 o3 o4 o5 o6 o7 ->
      let
        middle = (x0 + x1) / 2.0
        -- Octants with an odd index are on the high side of x.
        c7 = runFn4 thread o7 middle x1 after
        c6 = runFn4 thread o6 x0 middle c7
        c5 = runFn4 thread o5 middle x1 c6
        c4 = runFn4 thread o4 x0 middle c5
        c3 = runFn4 thread o3 middle x1 c4
        c2 = runFn4 thread o2 x0 middle c3
        c1 = runFn4 thread o1 middle x1 c2
        c0 = runFn4 thread o0 x0 middle c1
        sums =
          runFn3 accumulate o7 c7
            ( runFn3 accumulate o6 c6
                ( runFn3 accumulate o5 c5
                    ( runFn3 accumulate o4 c4
                        ( runFn3 accumulate o3 c3
                            (runFn3 accumulate o2 c2 (runFn3 accumulate o1 c1 (runFn3 accumulate o0 c0 (Sums 0.0 0.0 0.0 0.0 0.0))))
                        )
                    )
                )
            )
      in
        case sums of
          Sums strength weight x y z ->
            Cluster (strength * octantScale) (x / weight) (y / weight) (z / weight) (reach x0 x1) c0 after NoMembers

  reach x0 x1 = (x1 - x0) * (x1 - x0) / theta2

  -- The nodes of a leaf, newest first, built from the oldest up so that a
  -- leaf of many coincident nodes costs no stack.
  membersOf :: List (Point (Body row)) -> Members -> Members
  membersOf oldestFirst newer = case oldestFirst of
    Nil -> newer
    Cons point rest -> membersOf rest (Member point.item.index point.item.strength newer)

  -- `do strength += strengths[q.data.index]; while (q = q.next);`
  total :: Members -> Number -> Number
  total nodes sum = case nodes of
    NoMembers -> sum
    Member _ strength rest -> total rest (sum + strength)

-- | One octant of d3-force-3d's `accumulate`:
-- |
-- | ```
-- | if ((q = treeNode[i]) && (c = Math.abs(q.value)))
-- |   strength += q.value, weight += c, x += c * (q.x || 0), y += c * (q.y || 0), z += c * (q.z || 0);
-- | ```
-- |
-- | An empty octant's cluster is not its own (`thread` hands back whatever
-- | follows it), so the octant itself is asked first.
accumulate :: forall item. Fn3 (Octree item) Cluster Sums Sums
accumulate = mkFn3 \octant cluster sums -> case octant of
  Empty -> sums
  _ -> case cluster of
    Cluster strength x y z _ _ _ _ ->
      let
        weight = abs strength
      in
        if weight > 0.0 then case sums of
          Sums sumStrength sumWeight sumX sumY sumZ ->
            Sums (sumStrength + strength) (sumWeight + weight) (sumX + weight * orZero x) (sumY + weight * orZero y) (sumZ + weight * orZero z)
        else sums
    Finished -> sums

-- | `value || 0`.
orZero :: Number -> Number
orZero value = if value < 0.0 || value > 0.0 then value else 0.0

-- | What a walk leaves behind: the node's new velocity and the seed.
type Moved = { vx :: Number, vy :: Number, vz :: Number, seed :: Seed }

-- | `tree.visit(apply)` for one node: go along the chain, adding each
-- | cluster's pull to the velocity. The common cases are in the loop; the
-- | awkward distant cluster and the leaves go to `distant` and `near`.
walk :: Fn9 Walker Number Number Number Cluster Number Number Number Seed Moved
walk = mkFn9 \walker nodeX nodeY nodeZ chain startVx startVy startVz startSeed ->
  let
    alpha = walker.alpha
    distanceMin2 = walker.distanceMin2
    distanceMax2 = walker.distanceMax2

    go cluster vx vy vz seed = case cluster of
      Cluster strength centreX centreY centreZ reach inside after members ->
        -- `if (!treeNode.value) return true;`
        if not (strength < 0.0 || strength > 0.0) then go after vx vy vz seed
        else
          let
            x = centreX - nodeX
            y = centreY - nodeY
            z = centreZ - nodeZ
            distance2 = x * x + y * y + z * z
          in
            if reach < distance2 then
              -- Far enough away to count as one body.
              if distance2 < distanceMax2 then
                if distance2 >= distanceMin2 && x /= 0.0 && y /= 0.0 && z /= 0.0 then
                  go after
                    (vx + x * strength * alpha / distance2)
                    (vy + y * strength * alpha / distance2)
                    (vz + z * strength * alpha / distance2)
                    seed
                else
                  let
                    moved = runFn10 distant walker strength x y z distance2 vx vy vz seed
                  in
                    go after moved.vx moved.vy moved.vz moved.seed
              else go after vx vy vz seed
            else case members of
              -- Too close to approximate: open the cube.
              NoMembers -> go inside vx vy vz seed
              _ ->
                if distance2 >= distanceMax2 then go after vx vy vz seed
                else
                  let
                    moved = runFn10 near walker members x y z distance2 vx vy vz seed
                  in
                    go after moved.vx moved.vy moved.vz moved.seed
      Finished -> { vx, vy, vz, seed }
  in
    go chain startVx startVy startVz startSeed

-- | A distant cluster that cannot be taken in the loop's one line:
-- |
-- | ```
-- | if (x === 0) x = jiggle(random), l += x * x;
-- | if (y === 0) y = jiggle(random), l += y * y;
-- | if (z === 0) z = jiggle(random), l += z * z;
-- | if (l < distanceMin2) l = Math.sqrt(distanceMin2 * l);
-- | node.vx += x * treeNode.value * alpha / l;
-- | node.vy += y * treeNode.value * alpha / l;
-- | node.vz += z * treeNode.value * alpha / l;
-- | ```
distant :: Fn10 Walker Number Number Number Number Number Number Number Number Seed Moved
distant = mkFn10 \walker strength x y z distance2 vx vy vz seed ->
  let
    apart = runFn6 separate walker.distanceMin2 x y z distance2 seed
  in
    { vx: vx + apart.x * strength * walker.alpha / apart.distance2
    , vy: vy + apart.y * strength * walker.alpha / apart.distance2
    , vz: vz + apart.z * strength * walker.alpha / apart.distance2
    , seed: apart.seed
    }

-- | A leaf too close to approximate:
-- |
-- | ```
-- | if (treeNode.data !== node || treeNode.next) { ...separate as above... }
-- | do if (treeNode.data !== node) {
-- |   w = strengths[treeNode.data.index] * alpha / l;
-- |   node.vx += x * w; node.vy += y * w; node.vz += z * w;
-- | } while (treeNode = treeNode.next);
-- | ```
near :: Fn10 Walker Members Number Number Number Number Number Number Number Seed Moved
near = mkFn10 \walker members x y z distance2 vx vy vz seed -> case members of
  Member only _ NoMembers | only == walker.index -> { vx, vy, vz, seed }
  _ ->
    let
      apart = runFn6 separate walker.distanceMin2 x y z distance2 seed
    in
      pushFrom members walker.index walker.alpha apart vx vy vz

-- | A displacement made safe to divide by: jiggled where a component is
-- | exactly zero, and with its squared length raised to the minimum.
type Apart = { x :: Number, y :: Number, z :: Number, distance2 :: Number, seed :: Seed }

-- | ```
-- | if (x === 0) x = jiggle(random), l += x * x;
-- | if (y === 0) y = jiggle(random), l += y * y;
-- | if (z === 0) z = jiggle(random), l += z * z;
-- | if (l < distanceMin2) l = Math.sqrt(distanceMin2 * l);
-- | ```
separate :: Fn6 Number Number Number Number Number Seed Apart
separate = mkFn6 \distanceMin2 x y z distance2 seed ->
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
  in
    { x: apartX
    , y: apartY
    , z: apartZ
    , distance2: if lengthZ < distanceMin2 then sqrt (distanceMin2 * lengthZ) else lengthZ
    , seed: seedZ
    }

-- | The loop over the nodes of a leaf.
pushFrom :: Members -> Int -> Number -> Apart -> Number -> Number -> Number -> Moved
pushFrom nodes index alpha apart = go nodes
  where
  go remaining vx vy vz = case remaining of
    NoMembers -> { vx, vy, vz, seed: apart.seed }
    Member other strength rest ->
      if other == index then go rest vx vy vz
      else
        let
          pull = strength * alpha / apart.distance2
        in
          go rest (vx + apart.x * pull) (vy + apart.y * pull) (vz + apart.z * pull)

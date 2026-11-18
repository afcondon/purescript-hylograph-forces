-- | The many-body force: every node attracts or repels every other, with the
-- | far field approximated by Barnes-Hut over a quadtree.
-- |
-- | Ported from d3-force 3.0.0, `manyBody.js` (ISC, Copyright 2010-2021 Mike
-- | Bostock); see LICENSE. The arithmetic, its order and the draws from the
-- | random source follow d3 line for line, so the velocities agree with
-- | d3-force to the bit.
-- |
-- | Each application has three steps, and the first two are pure:
-- |
-- | 1. build the quadtree of the nodes' positions (`Quadtree.build`);
-- | 2. turn it into a chain of `Cluster`s: d3's `accumulate`, which gives
-- |    every square its total strength and centre, done together with the
-- |    threading that lets the walk run without a stack;
-- | 3. for each node in turn, walk the chain to get the node's new velocity,
-- |    and write it.
-- |
-- | The chain is the tree in d3's visiting order. Each cluster knows the
-- | cluster to go to when its square has to be opened (`inside`) and the one
-- | to go to when the square is finished with (`after`), so "visit, and skip
-- | the children if the callback says so" becomes a loop that follows one
-- | pointer or the other.
module Hylograph.Forces.Pure.ManyBody
  ( Config
  , defaultConfig
  , Scope
  , everyPair
  , withinGroups
  , betweenGroups
  , ManyBody
  , initialize
  , apply
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Function.Uncurried (Fn10, Fn3, Fn4, Fn9, mkFn10, mkFn3, mkFn4, mkFn9, runFn10, runFn3, runFn4, runFn9)
import Data.List (List(..), reverse)
import Data.Number (abs, infinity, sqrt)
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.Forces.Pure.Group (Membership(..), grouped)
import Hylograph.Forces.Pure.Node (Node, writeVelocity)
import Hylograph.Forces.Pure.Quadtree (Point, Quadtree(..))
import Hylograph.Forces.Pure.Quadtree as Quadtree
import Hylograph.Forces.Pure.Random (Seed)
import Hylograph.Forces.Pure.Random as Random

-- | d3's `forceManyBody` settings. `strength` is asked once per node when the
-- | force is initialized; negative repels.
type Config row =
  { strength :: Node row -> Number
  , theta :: Number
  , distanceMin :: Number
  , distanceMax :: Number
  , scope :: Scope
  , group :: Node row -> Membership
  }

-- | d3's defaults: strength -30, theta 0.9, distances from 1 to infinity,
-- | between every pair of nodes.
defaultConfig :: forall row. Config row
defaultConfig =
  { strength: \_ -> -30.0
  , theta: 0.9
  , distanceMin: 1.0
  , distanceMax: infinity
  , scope: EveryPair
  , group: \_ -> Ungrouped
  }

-- | Which pairs of nodes the force acts between. Only `EveryPair` is d3's;
-- | the other two are the Rust kernel's scoped many-body (`apply_many_body`
-- | in `force-kernel/src/forces.rs`), which `test/differential.mjs` holds
-- | this to bit for bit.
-- |
-- | - `WithinGroups`: for each group in ascending order of its number, d3's
-- |   many-body is applied to that group's nodes alone: its own quadtree,
-- |   its nodes in node order. A node in no group neither exerts nor feels
-- |   anything.
-- | - `BetweenGroups`: d3's many-body over all the nodes, followed by
-- |   `WithinGroups` with every strength negated. With theta 0 that is the
-- |   sum over pairs in different groups; with theta above 0 it is the
-- |   difference of two Barnes-Hut approximations. A node in no group
-- |   interacts with every node.
-- |
-- | The random source is drawn from in that order too.
data Scope
  = EveryPair
  | WithinGroups
  | BetweenGroups

everyPair :: Scope
everyPair = EveryPair

withinGroups :: Scope
withinGroups = WithinGroups

betweenGroups :: Scope
betweenGroups = BetweenGroups

-- | A node as the force sees it: its place in the node array and its
-- | strength.
type Body row = { index :: Int, strength :: Number, node :: Node row }

-- | The squared distances the walk compares against.
type Limits = { theta2 :: Number, distanceMin2 :: Number, distanceMax2 :: Number }

-- | An initialized many-body force: every node, and the nodes of each
-- | group as the scope wants them (with their strengths negated for
-- | `BetweenGroups`).
newtype ManyBody row = ManyBody
  { bodies :: Array (Body row)
  , groups :: Array (Array (Body row))
  , scope :: Scope
  , limits :: Limits
  , random :: Random.Source
  }

-- | d3's `force.initialize(nodes, random)`. A node's index is its place in
-- | the array, which is what d3's simulation sets `node.index` to.
initialize :: forall row. Config row -> Random.Source -> Array (Node row) -> ManyBody row
initialize config random nodes = ManyBody
  { bodies
  , groups: case config.scope of
      EveryPair -> []
      WithinGroups -> grouped (\body -> config.group body.node) bodies
      BetweenGroups -> grouped (\body -> config.group body.node) (map negated bodies)
  , scope: config.scope
  , limits:
      { theta2: config.theta * config.theta
      , distanceMin2: config.distanceMin * config.distanceMin
      , distanceMax2: config.distanceMax * config.distanceMax
      }
  , random
  }
  where
  bodies = mapWithIndex (\index node -> { index, strength: config.strength node, node }) nodes
  negated body = body { strength = -1.0 * body.strength }

-- | d3's `force(alpha)`: add the many-body acceleration to every node's
-- | velocity.
apply :: forall row. ManyBody row -> Number -> Effect Unit
apply (ManyBody force) alpha = case force.scope of
  EveryPair -> among force.bodies
  WithinGroups -> foreachE force.groups among
  BetweenGroups -> do
    among force.bodies
    foreachE force.groups among
  where
  -- d3's many-body among the given nodes alone.
  among bodies = do
    points <- snapshot bodies
    let chain = clusters force.limits.theta2 (Quadtree.build points)
    foreachE bodies \body -> do
      seed <- Ref.read force.random
      let moved = runFn9 walk force.limits alpha body.index body.node.x body.node.y chain body.node.vx body.node.vy seed
      runEffectFn3 writeVelocity body.node moved.vx moved.vy
      Ref.write moved.seed force.random

-- | Where the nodes are now, newest first. An effect, because the nodes are
-- | mutable: the positions are read when the force is applied, not when the
-- | application is described.
snapshot :: forall row. Array (Body row) -> Effect (List (Point (Body row)))
snapshot bodies = do
  points <- Ref.new Nil
  foreachE bodies \body -> do
    earlier <- Ref.read points
    Ref.write (Cons { x: body.node.x, y: body.node.y, item: body } earlier) points
  Ref.read points

-- | One square of the tree, ready for the walk. The fields are, in order:
-- |
-- | 1. the summed strength inside the square (d3's `quad.value`);
-- | 2. the x of the centre of that strength;
-- | 3. the y of the centre;
-- | 4. the reach, `w * w / theta2`: beyond this squared distance the square
-- |    acts as one body;
-- | 5. inside: where the walk goes to open the square;
-- | 6. after: where the walk goes when the square is done;
-- | 7. the nodes of a leaf, and none for a square with quarters.
-- |
-- | They sit in the constructor rather than in a record because the walk
-- | reads them a million times a tick and a record would be one more object
-- | to follow a pointer to.
data Cluster
  = Cluster Number Number Number Number Cluster Cluster Members
  | Finished

-- | The nodes at one location, newest first: index, strength, older nodes.
data Members
  = NoMembers
  | Member Int Number Members

-- | The whole tree as a chain of clusters.
clusters :: forall row. Number -> Quadtree.Planted (Body row) -> Cluster
clusters theta2 planted = runFn4 thread planted.root planted.extent.x0 planted.extent.x1 Finished
  where
  -- The chain for one square, which runs on into `after`. Quarters are
  -- threaded last to first so that each knows what follows it, then summed
  -- first to last, which is the order d3 adds them in.
  thread :: Fn4 (Quadtree (Body row)) Number Number Cluster Cluster
  thread = mkFn4 \tree x0 x1 after -> case tree of
    Empty -> after
    Leaf newest older ->
      let
        nodes = membersOf (reverse (Cons newest older)) NoMembers
      in
        Cluster (total nodes 0.0) newest.x newest.y ((x1 - x0) * (x1 - x0) / theta2) after after nodes
    Branch topLeft topRight bottomLeft bottomRight ->
      let
        middle = (x0 + x1) / 2.0
        fourth = runFn4 thread bottomRight middle x1 after
        third = runFn4 thread bottomLeft x0 middle fourth
        second = runFn4 thread topRight middle x1 third
        first = runFn4 thread topLeft x0 middle second
        -- d3's `accumulate`, for a square with quarters:
        --
        --   if ((q = quad[i]) && (c = Math.abs(q.value)))
        --     strength += q.value, weight += c, x += c * q.x, y += c * q.y;
        --
        -- A quarter that is absent, or whose strength is zero or NaN, has
        -- weight zero here and `counting` leaves the sum as it was.
        one = present topLeft first
        two = present topRight second
        three = present bottomLeft third
        four = present bottomRight fourth
        weightOne = weigh one
        weightTwo = weigh two
        weightThree = weigh three
        weightFour = weigh four
        weight = 0.0 + weightOne + weightTwo + weightThree + weightFour
        strength =
          runFn3 counting weightFour (strengthOf four)
            (runFn3 counting weightThree (strengthOf three) (runFn3 counting weightTwo (strengthOf two) (runFn3 counting weightOne (strengthOf one) 0.0)))
        x =
          runFn3 counting weightFour (weightFour * centreXOf four)
            ( runFn3 counting weightThree (weightThree * centreXOf three)
                (runFn3 counting weightTwo (weightTwo * centreXOf two) (runFn3 counting weightOne (weightOne * centreXOf one) 0.0))
            )
        y =
          runFn3 counting weightFour (weightFour * centreYOf four)
            ( runFn3 counting weightThree (weightThree * centreYOf three)
                (runFn3 counting weightTwo (weightTwo * centreYOf two) (runFn3 counting weightOne (weightOne * centreYOf one) 0.0))
            )
      in
        Cluster strength (x / weight) (y / weight) ((x1 - x0) * (x1 - x0) / theta2) first after NoMembers

  -- The cluster of a quarter, or `Finished` where the quarter is empty (for
  -- an empty quarter `thread` hands back whatever follows it, which is not
  -- the quarter's own).
  present :: Quadtree (Body row) -> Cluster -> Cluster
  present quarter cluster = case quarter of
    Empty -> Finished
    _ -> cluster

  -- The nodes of a leaf, newest first, built from the oldest up so that a
  -- leaf of ten thousand coincident nodes costs no stack.
  membersOf :: List (Point (Body row)) -> Members -> Members
  membersOf oldestFirst newer = case oldestFirst of
    Nil -> newer
    Cons point rest -> membersOf rest (Member point.item.index point.item.strength newer)

  -- d3: `do strength += strengths[q.data.index]; while (q = q.next);`
  total :: Members -> Number -> Number
  total nodes sum = case nodes of
    NoMembers -> sum
    Member _ strength rest -> total rest (sum + strength)

-- | d3's `c = Math.abs(q.value)` where it is truthy, and zero where it is
-- | not: the weight a quarter carries into its parent's centre.
weigh :: Cluster -> Number
weigh cluster = case cluster of
  Cluster strength _ _ _ _ _ _ ->
    let
      weight = abs strength
    in
      if weight > 0.0 then weight else 0.0
  Finished -> 0.0

-- | `sum + term` for a quarter that counts (weight above zero), and `sum`
-- | for one that does not.
counting :: Fn3 Number Number Number Number
counting = mkFn3 \weight term sum -> if weight > 0.0 then sum + term else sum

strengthOf :: Cluster -> Number
strengthOf cluster = case cluster of
  Cluster strength _ _ _ _ _ _ -> strength
  Finished -> 0.0

centreXOf :: Cluster -> Number
centreXOf cluster = case cluster of
  Cluster _ x _ _ _ _ _ -> x
  Finished -> 0.0

centreYOf :: Cluster -> Number
centreYOf cluster = case cluster of
  Cluster _ _ y _ _ _ _ -> y
  Finished -> 0.0

-- | What a walk leaves behind: the node's new velocity and the seed as the
-- | walk left it.
type Moved = { vx :: Number, vy :: Number, seed :: Seed }

-- | d3's `tree.visit(apply)` for one node: go along the chain, adding each
-- | cluster's pull to the velocity. This is the loop the whole simulation
-- | spends its time in. Its arguments are the limits, alpha, the node's index
-- | and position, the chain, and the velocity and seed to start from; they
-- | are taken all at once (`Fn9`) so that calling it does not build nine
-- | closures per node.
-- |
-- | The loop itself holds only what nearly every step does: skip a cluster
-- | with no strength, take a distant cluster as one body, or open a near
-- | one. The awkward distant cluster (coincident, or inside the minimum
-- | distance) and the leaves are handed to `distant` and `near`, which keeps
-- | the loop small enough for the JavaScript engine to compile it as one
-- | piece.
walk :: Fn9 Limits Number Int Number Number Cluster Number Number Seed Moved
walk = mkFn9 \limits alpha index nodeX nodeY chain startVx startVy startSeed ->
  let
    distanceMin2 = limits.distanceMin2
    distanceMax2 = limits.distanceMax2

    go cluster vx vy seed = case cluster of
      Cluster strength centreX centreY reach inside after members ->
        -- d3: `if (!quad.value) return true;`
        if not (strength < 0.0 || strength > 0.0) then go after vx vy seed
        else
          let
            x = centreX - nodeX
            y = centreY - nodeY
            distance2 = x * x + y * y
          in
            if reach < distance2 then
              -- Far enough away to count as one body.
              if distance2 < distanceMax2 then
                if distance2 >= distanceMin2 && x /= 0.0 && y /= 0.0 then
                  go after (vx + x * strength * alpha / distance2) (vy + y * strength * alpha / distance2) seed
                else
                  let
                    moved = runFn9 distant limits alpha strength x y distance2 vx vy seed
                  in
                    go after moved.vx moved.vy moved.seed
              else go after vx vy seed
            else case members of
              -- Too close to approximate: open the square.
              NoMembers -> go inside vx vy seed
              _ ->
                if distance2 >= distanceMax2 then go after vx vy seed
                else
                  let
                    moved = runFn10 near limits alpha index members x y distance2 vx vy seed
                  in
                    go after moved.vx moved.vy moved.seed
      Finished -> { vx, vy, seed }
  in
    go chain startVx startVy startSeed

-- | A distant cluster that cannot be taken in the loop's one line: d3's
-- |
-- | ```
-- | if (x === 0) x = jiggle(random), l += x * x;
-- | if (y === 0) y = jiggle(random), l += y * y;
-- | if (l < distanceMin2) l = Math.sqrt(distanceMin2 * l);
-- | node.vx += x * quad.value * alpha / l;
-- | node.vy += y * quad.value * alpha / l;
-- | ```
distant :: Fn9 Limits Number Number Number Number Number Number Number Seed Moved
distant = mkFn9 \limits alpha strength x y distance2 vx vy seed ->
  let
    apart = separate limits x y distance2 seed
  in
    { vx: vx + apart.x * strength * alpha / apart.distance2
    , vy: vy + apart.y * strength * alpha / apart.distance2
    , seed: apart.seed
    }

-- | A leaf too close to approximate: d3's
-- |
-- | ```
-- | if (quad.data !== node || quad.next) {
-- |   if (x === 0) x = jiggle(random), l += x * x;
-- |   if (y === 0) y = jiggle(random), l += y * y;
-- |   if (l < distanceMin2) l = Math.sqrt(distanceMin2 * l);
-- | }
-- | do if (quad.data !== node) {
-- |   w = strengths[quad.data.index] * alpha / l;
-- |   node.vx += x * w;
-- |   node.vy += y * w;
-- | } while (quad = quad.next);
-- | ```
near :: Fn10 Limits Number Int Members Number Number Number Number Number Seed Moved
near = mkFn10 \limits alpha index members x y distance2 vx vy seed -> case members of
  Member only _ NoMembers | only == index -> { vx, vy, seed }
  _ ->
    let
      apart = separate limits x y distance2 seed
      pushed = pushFrom members index alpha apart.x apart.y apart.distance2 vx vy
    in
      { vx: pushed.vx, vy: pushed.vy, seed: apart.seed }

-- | A displacement made safe to divide by: jiggled where a component is
-- | exactly zero, and with its squared length raised to the minimum.
type Apart = { x :: Number, y :: Number, distance2 :: Number, seed :: Seed }

-- | d3:
-- |
-- | ```
-- | if (x === 0) x = jiggle(random), l += x * x;
-- | if (y === 0) y = jiggle(random), l += y * y;
-- | if (l < distanceMin2) l = Math.sqrt(distanceMin2 * l);
-- | ```
separate :: Limits -> Number -> Number -> Number -> Seed -> Apart
separate limits x y distance2 seed =
  let
    seedX = if x == 0.0 then Random.advance seed else seed
    apartX = if x == 0.0 then Random.jiggle seedX else x
    lengthX = if x == 0.0 then distance2 + apartX * apartX else distance2
    seedY = if y == 0.0 then Random.advance seedX else seedX
    apartY = if y == 0.0 then Random.jiggle seedY else y
    lengthY = if y == 0.0 then lengthX + apartY * apartY else lengthX
  in
    { x: apartX
    , y: apartY
    , distance2: if lengthY < limits.distanceMin2 then sqrt (limits.distanceMin2 * lengthY) else lengthY
    , seed: seedY
    }

-- | d3's loop over the nodes of a leaf:
-- |
-- | ```
-- | do if (quad.data !== node) {
-- |   w = strengths[quad.data.index] * alpha / l;
-- |   node.vx += x * w;
-- |   node.vy += y * w;
-- | } while (quad = quad.next);
-- | ```
pushFrom :: Members -> Int -> Number -> Number -> Number -> Number -> Number -> Number -> { vx :: Number, vy :: Number }
pushFrom nodes index alpha x y distance2 = go nodes
  where
  go remaining vx vy = case remaining of
    NoMembers -> { vx, vy }
    Member other strength rest ->
      if other == index then go rest vx vy
      else
        let
          pull = strength * alpha / distance2
        in
          go rest (vx + x * pull) (vy + y * pull)


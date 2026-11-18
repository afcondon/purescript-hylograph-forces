# hylograph-forces-pure

**A spike, not a release.** d3-force's forces written in PureScript, with no
JavaScript for any algorithm, held to d3-force 3.0.0 bit for bit and timed
beside it; and the six forces the Rust/WASM kernel has that d3 does not,
held bit for bit to that kernel built at 64 bits. Unpublished; nothing
depends on it.

The question behind it: can Hylograph stop depending on d3-force without
going to the Rust/WASM kernel? The answer measured here is in
[Results](#results).

## What is in it

| Module | d3 source | What |
|---|---|---|
| `Hylograph.Forces.Pure` | `simulation.js` | one `Force` type over all seven, `applyForces`, and `tick` (cool, apply, move) |
| `…Pure.ManyBody` | `manyBody.js` | Barnes-Hut many-body: theta, distanceMin, distanceMax, per-node strength |
| `…Pure.Link` | `link.js` | springs: d3's degree-based strength and bias, per-link distance and strength, iterations |
| `…Pure.Collide` | `collide.js` | circle collisions: per-node radius, strength, iterations |
| `…Pure.Center` | `center.js` | centring |
| `…Pure.Position` | `x.js`, `y.js` | pull towards an x or a y |
| `…Pure.Radial` | `radial.js` | pull towards a circle |
| `…Pure.Tick` | `simulation.js` | alpha decay; velocity decay and position update, honouring `fx` / `fy` |
| `…Pure.Quadtree` | d3-quadtree | the quadtree both tree forces are built on |
| `…Pure.Random` | `lcg.js`, `jiggle.js` | d3's seeded generator and its jiggle |
| `…Pure.Linkage` | `link.js` | links joined to their nodes, by place or by id (d3's error for a missing node) |
| `…Pure.Node` | | the node type, and the package's only foreign code |
| `…Pure.Towards` | kernel | a spring from each node to its own target point |
| `…Pure.RectCollide` | kernel | axis-aligned rectangles pushed apart along the axis of least overlap |
| `…Pure.Contain` | kernel | keep nodes inside a rectangle, circle or convex polygon: soft force, optional hard clamp, region per node |
| `…Pure.Cohesion` | kernel | pull each node toward its group's centroid |
| `…Pure.ManyBody` (`Scope`) | kernel | many-body within groups only, or between groups only |
| `…Pure.Directional` | kernel | keep each link's target a gap beyond its source along an axis |
| `…Pure.Collide` (`include`, `alphaScaled`) | kernel | a collision that leaves some nodes out, or weakens as the simulation cools |
| `…Pure.Snapshot`, `…Pure.Measure` | kernel | snapshot and restore; kinetic energy, largest displacement, largest acceleration, `settled` |
| `…Pure.Group` | | which group a node is in |

"kernel" means the rule is the one written on the matching `apply_*`
function in `purescript-hylograph-showcases/wasm-force-demo/force-kernel/src/forces.rs`.
`…Pure.Tick` also has d3's `initializeNodes` (index, pins, phyllotaxis for
a node with no position) and the cooling step as `ForceEngine.Core` has it.

It fits the loop `Hylograph.ForceEngine.Core` already runs round d3's
functions: nodes are the same records (`x`, `y`, `vx`, `vy`, optionally `fx`,
`fy`), updated in place; a force is initialized against the node array and
then applied once per tick with alpha.

2,899 lines of PureScript, 1,839 of them code (the d3 part was 1,490 and 921). Four lines of JavaScript.

### The foreign code

```js
export const writeVelocity = (node, vx, vy) => { node.vx = vx; node.vy = vy; };
export const writePosition = (node, x, y) => { node.x = x; node.y = y; };
export const writePin = (node, fx, fy) => { node.fx = fx; node.fy = fy; };
export const writeIndex = (node, index) => { node.index = index; };
```

The forces use the first two. `writePin` is for restoring a snapshot and
`writeIndex` for `initializeNodes`.

PureScript can read a record field and cannot assign one, and I found
nothing in package set 80.8.0 that assigns one in place either (`Record.ST`
left the `record` package at 4.0). Everything else, every sum, comparison, square root and draw from
the random source, is PureScript.

## Checking it

```bash
spago build                          # the type check, and ./output
npm install && npm run build:es      # ./output-es, through purs-backend-es
npm run build:kernel                 # the Rust kernel at f64 from its current source, into test/kernel-f64
node test/differential.mjs           # against d3-force and the kernel, bit for bit
                                     #   --output output-es   --quick   --only d3 | kernel
node test/timing.mjs --output output,output-es
node test/timing-new.mjs             # the forces d3 does not have, beside the WASM kernel
```

The scripts read d3-force from `~/node_modules/d3-force/src`. The kernel
half of the test (`test/kernel-oracle.mjs`) needs the f64 build; without
`build:kernel` it uses the copy the crate's own test keeps in
`force-kernel/target/pkg-f64`, and says which it used.

## Results

### Agreement with d3-force

`test/differential.mjs` runs the same nodes, links and settings through
d3-force's functions and through the compiled PureScript and compares `x`,
`y`, `vx`, `vy` of every node after every tick with `Object.is`: no
tolerance, and `-0` is not `+0`.

| Force | Scenarios | Largest | Values compared | Stock `purs` | `purs-backend-es` |
|---|---|---|---|---|---|
| many-body | 352 | 12,000 nodes | 93,696,000 | identical | identical |
| link | 210 | 1,000 | 56,095,200 | identical | identical |
| collide | 212 | 12,000 | 56,299,200 | identical | identical |
| centre | 140 | 1,000 | 37,396,800 | identical | identical |
| x | 140 | 1,000 | 37,396,800 | identical | identical |
| y | 140 | 1,000 | 37,396,800 | identical | identical |
| radial | 140 | 1,000 | 37,396,800 | identical | identical |
| combinations (incl. many-body + link + centre at 10,000) | 352 | 10,000 | 109,492,000 | identical | identical |
| random graphs, fixed seed | 400 | 150 | 34,872,000 | identical | identical |
| against `d3.forceSimulation` | 45 | 1,000 | 19,980,000 | identical | identical |
| `initializeNodes` + links by id, against `d3.forceSimulation` | 6 | 1,000 | 2,006,166 | identical | identical |

2,137 scenarios and 522 million values per backend, and no tick at which any
value differs (re-run in full after the second half was added: nothing
regressed). Almost all of those values are finite: the only `NaN`s are in
the two scenarios that put one node at `NaN` on purpose (the script counts
them).

Every scenario is 300 ticks, except the two with 12,000 nodes on one spot
(3 ticks) and the 10,000-node run with collisions (100). The layouts include all nodes on
one spot, an integer lattice (many nodes exactly coincident, many pairs with
exactly equal x or y), a horizontal line and coincident pairs, so the jiggle
and the draws from the seeded generator are exercised in every force that
has them; the random graphs allow self-links and repeated links, pinned
nodes, a non-zero alpha target and other velocity decays. The last group
uses the real `d3.forceSimulation` as the reference rather than a loop round
d3's force functions.

### Time per tick

run.mjs's graph and forces (ring with chords on d3's spiral; many-body +
link + centre), 300 ticks, Node 22 on an M1 Pro that was never idle (load
average 8 to 50 from other work over the morning; about 10 for the run
below). To survive that, each contender's 300 ticks are timed in blocks of
ten over several interleaved runs and each block counts at its fastest;
d3-force is measured the same way in the same process. The WASM column is
the figure quoted for `wasm-kernel/run.mjs`; two single runs of it on the
same morning gave 0.05 to 0.07, 0.85 to 0.88 and 13.1 to 13.5 ms.

| Nodes | d3-force | this, stock `purs` | this, `purs-backend-es` | Rust/WASM kernel |
|---|---|---|---|---|
| 100 | 0.138 ms | 0.162 ms (1.18×) | 0.094 ms (0.68×) | 0.04 ms |
| 1,000 | 2.73 ms | 2.56 ms (0.94×) | 1.59 ms (0.58×) | 0.8 ms |
| 10,000 | 45.3 ms | 42.0 ms (0.93×) | 28.3 ms (0.62×) | 12 ms |

In brackets, time over d3-force's. Both builds of the port end the 300
ticks with every node where d3-force puts it, to the bit; the timing script
checks.

The collision force alone (radius 3 to 7, with the position update), same
method, three runs:

| Nodes | d3-force | this, stock `purs` | this, `purs-backend-es` |
|---|---|---|---|
| 1,000 | 0.90 ms | 0.98 ms (1.09×) | 0.51 ms (0.57×) |
| 10,000 | 12.8 ms | 15.2 ms (1.19×) | 8.9 ms (0.69×) |

So: under the stock backend the port is level with d3-force (a little
faster on the many-body force that dominates a tick, a little slower on
collisions and at a hundred nodes); under the optimising backend it takes
six tenths of d3-force's time; the WASM kernel takes a little over a quarter
of it.

### Agreement with the Rust kernel

For the forces d3 does not have, the oracle is the Rust kernel built at f64.
Same inputs through both; after every tick `Object.is` on x, y, vx, vy of
every node and on kinetic energy, largest displacement, largest
acceleration and `settled`. Every scenario
also snapshots a third of the way through on both sides, runs to 300 ticks,
restores, runs the same ticks again and must end in the same place.

| Force | Scenarios | Largest | Values compared | Nothing moved in | Stock `purs` | `purs-backend-es` |
|---|---|---|---|---|---|---|
| towards | 210 | 1,000 nodes | 94,004,652 | 8 | identical | identical |
| rectangle collision | 210 | 1,000 | 94,004,652 | 58 | identical | identical |
| containment | 560 | 1,000 | 250,679,072 | 108 | identical | identical |
| cohesion | 210 | 1,000 | 94,004,652 | 71 | identical | identical |
| scoped many-body | 490 | 1,000 | 219,344,188 | 91 | identical | identical |
| yielding collide (filter, strength following alpha) | 280 | 1,000 | 125,339,536 | 99 | identical | identical |
| directional | 210 | 1,000 | 94,004,652 | 39 | identical | identical |
| new and d3 forces combined | 352 | 10,000 | 164,715,212 | 21 | identical | identical |
| random graphs, fixed seed, any of twelve kinds of force | 400 | 150 | 61,507,048 | 2 | identical | identical |

2,922 scenarios and 1.2 billion values per backend, none of them `NaN` or
infinite, and no tick at which any value differs. Snapshot and restore, and
the four measures, are checked inside every one of them. The kernel was built
from its source as committed on 2026-11-18 (`794aa17`).

The inputs are chosen to be awkward: every node on one spot; an integer
lattice with region edges and polygon vertices on lattice points (nodes
exactly on a boundary and exactly on a vertex); rectangles of zero width
and zero area, both as collision boxes and as regions; a polygon of zero
area and one of two points; a disc smaller than the nodes kept in it; a
rectangle given corner-for-corner backwards; group numbers with gaps, one
group, no groups; a zero axis; pinned nodes fighting a hard region.
"Nothing moved" counts the scenarios where the forces had nothing to do, in
which agreement is no evidence.

Kept out of the inputs, because there the kernel deliberately departs from
d3 and the port follows d3: positions or velocities that are not finite,
`NaN` strengths for many-body, `NaN` targets for x, y and radial.

**No case was found where the Rust departs from its written rule.** Scoped
many-body does depend on order (the random source is drawn from as the
trees are walked): groups in ascending order of their number, each group's
nodes in node order, each over its own quadtree in d3's visiting order; for
"between", the pass over all nodes first. That is the order the Rust
documents and the order used here.

Where the written rule was not enough and the Rust code had to be read:

- `NaN` handling is in helper functions, not in the rules: a `NaN`
  strength, radius or half-extent counts as zero; a link whose gap or
  strength is `NaN` is skipped by the directional constraint.
- Rectangle regions are normalised (corners in either order) by the
  constructor; `project`'s rule assumes `x0 <= x1`.
- A polygon edge of zero length is dropped; only "fewer than three
  vertices or zero area" is written down.
- Rectangle collision's share is written as "1/2 if A_i + A_j = 0"; the
  code is "1/2 unless A_i + A_j > 0", which differs for negative
  half-extents.
- The order of a tick (hard containment after the position update, then
  pins again) and that forces see the alpha from *before* cooling are in
  `lib.rs`'s header, not with the rules. d3 cools first, so the kernel's
  n-th tick runs at d3's (n-1)-th alpha; the test starts the kernel one
  cooling step ahead to line them up.

The five things the kernel gained after the first six (a collision with a
node filter, a collision whose strength follows alpha, `max_acceleration`,
`settled`, and `velocity_retention` as the name of what a velocity keeps)
are ported and in the table above ("yielding collide"; the two measures are
compared after every tick of every scenario).

### Time per tick, the forces d3 does not have

Each force alone with the position update, run.mjs's graph, first 100
ticks, three interleaved runs, each block of ten ticks at its fastest; the
WASM kernel is the shipped f32 build. Two differential runs were going on
the same machine at the time.

ms per tick at 100 / 1,000 / 10,000 nodes:

| Force | WASM kernel (f32) | this, stock `purs` | this, `purs-backend-es` |
|---|---|---|---|
| towards | 0.002 / 0.009 / 0.07 | 0.005 / 0.05 / 0.47 | 0.001 / 0.012 / 0.11 |
| rectangle collision | 0.02 / 0.44 / 5.6 | 0.09 / 1.4 / 22 | 0.06 / 0.88 / 16 |
| containment (disc, soft + hard) | 0.002 / 0.018 / 0.18 | 0.02 / 0.15 / 1.6 | 0.007 / 0.06 / 0.71 |
| cohesion (8 groups) | 0.002 / 0.019 / 0.19 | 0.01 / 0.08 / 0.88 | 0.003 / 0.03 / 0.35 |
| many-body within 8 groups | 0.02 / 0.52 / 7.7 | 0.12 / 1.9 / 30 | 0.06 / 1.0 / 18 |
| many-body between 8 groups | 0.06 / 1.25 / 18 | 0.30 / 4.5 / 69 | 0.16 / 2.6 / 48 |
| directional | 0.002 / 0.02 / 0.20 | 0.007 / 0.08 / 0.72 | 0.006 / 0.04 / 0.41 |
| all seven together | 0.18 / 3.1 / 43 | 0.95 / 16 / 220 | 0.58 / 9.7 / 145 |

The WASM kernel is 2 to 4 times faster than the port under the optimising
backend, and 4 to 9 times faster than it under the stock backend. None of these loops has had the attention the
many-body walk had in the first half; rectangle collision in particular
sorts a list per node.

## How it is built, and why

**The tree is built without mutation.** d3 inserts points one at a time into
a mutable quadtree. `Quadtree.build` deals the points into quarters from the
top down, which arrives at the same tree: d3 splits a square exactly when a
second location lands in it, so the shape does not depend on insertion order.
The two things that do depend on it are kept: a leaf's coincident points run
newest first, and the extent grows from the unit square at the floor of the
smallest coordinates.

**The walk has no stack.** Each tree force turns the tree into a chain of
squares in d3's visiting order, where every square knows the square to go to
if it has to be opened and the one to go to when it is done with. d3's
"visit, and skip the children if the callback returns true" becomes a
tail-recursive function that follows one pointer or the other, and both
backends compile that to a `while` loop over local variables. Building the
chain is also where d3's `visitAfter` pass (summed strength and centre for
many-body, largest radius for collide) is done.

**The walks are pure.** The random source is a seed threaded through the
loop, so a many-body walk is a function from a node and the chain to a new
velocity. A collide walk returns the node's new velocity and the new
velocities of the nodes it pushed; that is the same as d3 writing them as it
goes, because a walk meets each other node once and never reads back what it
changed.

**Nodes are mutable behind the type system's back,** so two rules are kept
and can be checked in the generated JavaScript. A mutable field is read
where its value is used, in the arguments of the write that replaces it or
inside a pure function that returns before anything is written. And reading
the nodes is part of the effect: `apply force alpha` builds nothing until
the effect runs. (The first version let-bound the tree outside the effect,
and both backends duly built it when the application was described rather
than when it ran.)

**Records, not arrays.** This is the main thing the spike found out. The
stock compiler emits a native field read for a record or a constructor, but
every array read is a call to `Data.Array.unsafeIndex`: curried, through a
library function whose one indexing instruction is shared with every other
array read in the program. So the hot structures are constructors with positional fields and
the hot loop touches no array at all. Typed arrays were ruled out sooner:
the `arraybuffer` package can copy an array in or fill a range, but has no
plain store of one element.

## Optimisation log

Ratios are time per tick over d3-force's in the same process, 10,000 nodes,
many-body + link + centre, first 100 ticks; lower is better.

| Step | stock | purs-backend-es |
|---|---|---|
| 1. Straightforward port: tree as an ADT, chain of clusters holding a record each, curried walk | 1.07 | 0.58 |
| 2. Tried: the chain as columns (struct of arrays, read by `unsafeIndex`). Not kept | 1.39 | 0.55 |
| 3. Cluster fields in the constructor instead of a record inside it; walk taken as `Fn9` | 0.89 | 0.58 |
| 4. Walk loop cut down to its three common cases, the rest handed to two helpers; `thread` and `cell` as `Fn4` / `Fn6`; the parent's sums as scalars rather than four records | 0.83 | 0.51 |
| 5. Same treatment for collide, link (no thunk per link in the common case), integrate (two null tests per node, not four) | see below | see below |
| 6. Collide walk split in two: `seek`, a loop whose only variable is the place in the chain, finds the next overlapping leaf; the outer loop runs once per collision | see below | see below |

Steps 5 and 6, each force alone at 10,000 nodes (with the position update),
ms per tick, first 100 ticks: collide 22.1 → 17.0 → 15.2 stock and 11.0 →
8.5 es, where step 6 changed nothing (d3 12.9); link 1.02 → 0.75 and 0.54 → 0.44 (d3 0.29); centre
0.83 → 0.66 and 0.24 → 0.14 (d3 0.07).

What the steps showed:

- **Step 2** was the surprise. Flat columns were meant to be the fast
  layout; under the optimising backend, where `unsafeIndex` is a bare
  `xs[i]`, they bought 4% in all (the walk itself 16%, less the cost of
  filling the columns), and under the stock backend they cost 30%. The walk
  is not waiting on memory: at about 90 squares visited per node it spends
  its time on the two divisions d3's arithmetic requires and on a branch
  that cannot be predicted.
- **Step 3** is the stock backend's one large gain: a record inside a
  constructor is a second object behind a second pointer, and nine curried
  arguments are nine closures per node.
- **Steps 4 and 6**: the stock backend compiles a tail-recursive loop as a
  function called once per turn, which copies every loop variable back to
  the enclosing scope on every turn, and V8 only inlines that function
  while it is small. So a stock-backend loop wants a small body and few
  variables. The optimising backend's loops do not care.
- **purs-backend-es** turns `STRef`s into `let`, `foreachE` into `for … of`,
  `Ref` reads and writes into field accesses, and uncurries what it can.
  It is worth about 1.5× over the stock backend on this code with no change
  to the source. One trap: it folds a recursive
  `if … then recurse else false` into `&&` and then does not make it a loop,
  so a function that was stack-safe under the stock backend overflowed at
  twelve thousand coincident nodes. `Quadtree.elsewhere` is written to avoid
  that, and the differential test now has that scenario.
- **Not done**: the tree build still allocates a list cell per point per
  level, and garbage collection is 11 to 14% of a tick. The cheap forces
  (link, centre, x, y, radial) are 2 to 10 times slower than d3 per node
  under the stock backend, because `foreachE` calls a closure per element;
  they are a few percent of a tick beside many-body.

### A second trap in the optimising backend

`purs-backend-es` treats multiplication of `Number`s as associative. It
compiled `toNumber index * (pi * (3.0 - sqrt 5.0))` as
`index * 3.141592653589793 * (3.0 - sqrt(5.0))`, which is a different
number, and d3's phyllotaxis placement came out wrong in the thirteenth
digit under that backend only. The differential test caught it; the fix is
to give the inner product a name of its own (`Tick.initialAngle`), as d3
does. No other expression in the package has the shape `a * (b * c)` or
`a + (b + c)`, and both backends pass the whole test, but anything added
later is exposed to it.

## Where it departs from d3

- A node's index is its place in the array (what `d3.forceSimulation` sets
  `node.index` to); the `index` field is written by `initializeNodes` and
  not read by the forces.
- Accessors (`strength`, `radius`, `distance`, …) take the node or link
  only, not d3's `(d, i, data)`.
- Links name their nodes by place (`Link.initialize`: a link naming a place
  outside the array is dropped) or by id (`Link.initializeById`: d3's
  `node not found: <id>` as a `Left`, where d3 throws).
- Like d3, the collision force looks only at the newest of several nodes at
  exactly the same place.

## In hylograph-simulation

This is the default kernel of `purescript-hylograph-simulation` (a path
dependency there, since this package is unpublished): its
`Hylograph.ForceEngine.Core` wraps a `Force` as a recipe and a handle, and
`Hylograph.ForceEngine.Setup.recipe` turns each force of a `Setup` into one.
That library's `test/engine-parity.mjs` holds this kernel and the WASM kernel
to each other through the one `Setup` vocabulary, and its README has the
timings through its own loop.

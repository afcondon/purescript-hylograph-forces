// Does the three-dimensional PureScript kernel give the numbers d3-force-3d
// gives at three dimensions? The same bits.
//
//   node test/differential-3d.mjs [--output output|output-es] [--quick]
//
// Every scenario runs the same nodes, links and settings through
// d3-force-3d's own functions (read from this package's
// node_modules/d3-force-3d/src, after `npm install`), initialized with
// numDimensions 3, and through the compiled PureScript of
// Hylograph.Forces.ThreeD (read from ./output, or ./output-es for the
// optimising backend), and compares x, y, z, vx, vy and vz of every node after
// every tick with Object.is: -0 is not +0, and no tolerance anywhere. It
// reports, for each group, whether any value differed, and for each scenario
// that differed the first tick, node and field, with both values.
//
// The reference loop is d3-force-3d's `simulation.tick` written out: cool
// alpha, call each force, then move the nodes (honouring fx, fy, fz). Two
// last groups use the real `forceSimulation` with three dimensions instead.
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const argument = (name, fallback) => {
  const at = process.argv.indexOf(name);
  return at < 0 ? fallback : process.argv[at + 1];
};
const outputDirectory = argument('--output', 'output');
const quick = process.argv.includes('--quick');
const ticksPerScenario = 300;

const d3 = await import(pathToFileURL(join(here, '..', 'node_modules/d3-force-3d/src/index.js')));
const { default: d3Lcg } = await import(pathToFileURL(join(here, '..', 'node_modules/d3-force-3d/src/lcg.js')));
const d3Version = (await import(pathToFileURL(join(here, '..', 'node_modules/d3-force-3d/package.json')), { with: { type: 'json' } })).default.version;
const compiled = (module) => import(pathToFileURL(join(here, '..', outputDirectory, module, 'index.js')));
const Pure = await compiled('Hylograph.Forces.ThreeD');
const PureLink = await compiled('Hylograph.Forces.ThreeD.Link');
const PureRandom = await compiled('Hylograph.Forces.Random');
const dimensions = 3;

// A small seeded generator for the test's own choices (mulberry32), so that
// every run of this file tests the same graphs.
const mulberry = (seed) => () => {
  seed = (seed + 0x6d2b79f5) | 0;
  let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
  t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};

// ---------------------------------------------------------------------------
// Layouts: where the nodes start.
// ---------------------------------------------------------------------------

const rollAngle = Math.PI * (3 - Math.sqrt(5));
const yawAngle = Math.PI * 20 / (9 + Math.sqrt(221));

const layouts = {
  // d3-force-3d's own initial layout at three dimensions.
  spiral: () => (i) => {
    const r = 10 * Math.cbrt(0.5 + i), roll = i * rollAngle, yaw = i * yawAngle;
    return [r * Math.sin(roll) * Math.cos(yaw), r * Math.cos(roll), r * Math.sin(roll) * Math.sin(yaw)];
  },
  scattered: (random) => () => [(random() - 0.5) * 400, (random() - 0.5) * 400, (random() - 0.5) * 400],
  // Every node on the same spot: nothing but jiggle can separate them.
  heap: () => () => [0, 0, 0],
  heapElsewhere: () => () => [12.5, -7.25, 3.5],
  // Integer coordinates on a small lattice: many exactly coincident nodes,
  // and many pairs with exactly equal x, y or z.
  lattice: (random) => () => [Math.floor(random() * 4), Math.floor(random() * 4), Math.floor(random() * 4)],
  // All on one line along x: every y and z difference is exactly zero, and
  // the octree's cells are flat in two directions.
  line: () => (i) => [i * 3, 5, -2],
  // All on the diagonal: the octree splits only the cells along it.
  diagonal: () => (i) => [i * 2 - 7, i * 2 - 7, i * 2 - 7],
  // All on one plane, z = 4: every z difference is zero.
  plane: (random) => () => [(random() - 0.5) * 300, (random() - 0.5) * 300, 4],
  // Pairs of nodes sharing a spot, the pairs spread out.
  pairs: () => (i) => [(i >> 1) * 7 - 20, ((i >> 1) % 5) * 11, ((i >> 1) % 3) * 5],
};

const makeNodes = (count, layout, random, { moving = false, pinned = 0 } = {}) => {
  const place = layouts[layout](random);
  return Array.from({ length: count }, (_, index) => {
    const [x, y, z] = place(index);
    const velocity = () => (moving ? (random() - 0.5) * 4 : 0);
    const node = { index, x, y, z, vx: velocity(), vy: velocity(), vz: velocity() };
    if (random() < pinned) node.fx = x + 1;
    if (random() < pinned) node.fy = y - 1;
    if (random() < pinned) node.fz = z + 0.5;
    return node;
  });
};

// run.mjs's graph: a ring with chords.
const ringWithChords = (count) => {
  const links = [];
  for (let i = 0; i < count; i++) {
    links.push([i, (i + 1) % count]);
    if (i % 7 === 0) links.push([i, (i * 13 + 5) % count]);
  }
  return links.filter(([a, b]) => a !== b).map(([source, target], i) => ({ source, target, length: 20 + (i % 5) * 6, weight: 0.2 + (i % 4) * 0.2 }));
};

const randomLinks = (count, howMany, random) =>
  Array.from({ length: howMany }, () => ({
    source: Math.floor(random() * count),
    target: Math.floor(random() * count), // self-links and repeats allowed
    length: 5 + random() * 60,
    weight: random() * 1.2,
  }));

// ---------------------------------------------------------------------------
// Forces: each description builds the d3-force-3d force and the PureScript one.
// ---------------------------------------------------------------------------

const always = (value) => () => value;

const forces = {
  manyBody: ({ strength = always(-30), theta = 0.9, distanceMin = 1, distanceMax = Infinity } = {}) => ({
    d3: () => d3.forceManyBody().strength(strength).theta(theta).distanceMin(distanceMin).distanceMax(distanceMax),
    pure: (random, nodes) => Pure.manyBody({ strength, theta, distanceMin, distanceMax })(random)(nodes),
  }),
  link: ({ distance = always(30), strength = null, iterations = 1 } = {}) => ({
    d3: (links) => {
      const force = d3.forceLink(links.map((link) => ({ ...link }))).distance(distance).iterations(iterations);
      return strength ? force.strength(strength) : force;
    },
    pure: (random, nodes, links) =>
      Pure.link({ distance, strength: strength ? PureLink.strengthPerLink(strength) : PureLink.strengthFromDegree, iterations })(random)(nodes)(links),
  }),
  collide: ({ radius = always(1), strength = 1, iterations = 1 } = {}) => ({
    d3: () => d3.forceCollide().radius(radius).strength(strength).iterations(iterations),
    pure: (random, nodes) => Pure.collide({ radius, strength, iterations })(random)(nodes),
  }),
  center: ({ x = 0, y = 0, z = 0, strength = 1 } = {}) => ({
    d3: () => d3.forceCenter(x, y, z).strength(strength),
    pure: (random, nodes) => Pure.center({ x, y, z, strength })(nodes),
  }),
  x: ({ target = always(0), strength = always(0.1) } = {}) => ({
    d3: () => d3.forceX().x(target).strength(strength),
    pure: (random, nodes) => Pure.positionX({ target, strength })(nodes),
  }),
  y: ({ target = always(0), strength = always(0.1) } = {}) => ({
    d3: () => d3.forceY().y(target).strength(strength),
    pure: (random, nodes) => Pure.positionY({ target, strength })(nodes),
  }),
  z: ({ target = always(0), strength = always(0.1) } = {}) => ({
    d3: () => d3.forceZ().z(target).strength(strength),
    pure: (random, nodes) => Pure.positionZ({ target, strength })(nodes),
  }),
  radial: ({ radius = always(100), strength = always(0.1), x = 0, y = 0, z = 0 } = {}) => ({
    d3: () => d3.forceRadial(radius, x, y, z).strength(strength),
    pure: (random, nodes) => Pure.radial({ radius, strength, x, y, z })(nodes),
  }),
};

// The groups whose force the PureScript does not have yet are left out, and
// the report says so.
const ported = { manyBody: typeof Pure.manyBody === 'function', collide: typeof Pure.collide === 'function' };
const needs = (build) => {
  const text = typeof build === 'string' ? build : build.toString();
  return (!text.includes('manyBody') || ported.manyBody) && (!text.includes('collide') || ported.collide);
};

// ---------------------------------------------------------------------------
// Running one scenario.
// ---------------------------------------------------------------------------

const fields = ['x', 'y', 'z', 'vx', 'vy', 'vz'];
const defaults = { alphaTarget: 0, alphaDecay: 1 - Math.pow(0.001, 1 / 300), velocityRetention: 0.6 };

// The first tick (from 0) at which the two disagree, or -1.
const run = ({ nodes, links = [], forceList, settings = defaults, ticks = ticksPerScenario, reference = 'loop' }) => {
  const forD3 = nodes.map((node) => ({ ...node }));
  const forPure = nodes.map((node) => ({ ...node }));
  let stepD3;
  if (reference === 'simulation') {
    // The real thing: forceSimulation, given three dimensions by
    // numDimensions(3), stopped and ticked by hand. Creating it snaps pinned
    // nodes to their pins, which is not part of a tick, so the other copy
    // gets the same treatment here.
    const simulation = d3.forceSimulation(forD3).numDimensions(dimensions).stop();
    forceList.forEach((force, i) => simulation.force(`force${i}`, force.d3(links)));
    for (const node of forPure) {
      if (node.fx != null) node.x = node.fx;
      if (node.fy != null) node.y = node.fy;
      if (node.fz != null) node.z = node.fz;
    }
    stepD3 = () => simulation.tick();
  } else {
    const random = d3Lcg();
    const built = forceList.map((force) => force.d3(links));
    for (const force of built) force.initialize(forD3, random, dimensions);
    let alpha = 1;
    stepD3 = () => {
      alpha += (settings.alphaTarget - alpha) * settings.alphaDecay;
      for (const force of built) force(alpha);
      for (const node of forD3) {
        if (node.fx == null) node.x += node.vx *= settings.velocityRetention; else node.x = node.fx, node.vx = 0;
        if (node.fy == null) node.y += node.vy *= settings.velocityRetention; else node.y = node.fy, node.vy = 0;
        if (node.fz == null) node.z += node.vz *= settings.velocityRetention; else node.z = node.fz, node.vz = 0;
      }
    };
  }
  const random = PureRandom.lcg();
  const built = forceList.map((force) => force.pure(random, forPure, links));
  let alpha = 1;
  let compared = 0, notFinite = 0;
  for (let tick = 0; tick < ticks; tick++) {
    stepD3();
    alpha = Pure.tick(settings)(built)(forPure)(alpha)();
    for (let i = 0; i < forD3.length; i++) {
      for (const field of fields) {
        compared++;
        if (!Number.isFinite(forD3[i][field])) notFinite++;
        if (!Object.is(forD3[i][field], forPure[i][field])) {
          return { first: tick, compared, notFinite, detail: `node ${i} ${field}: d3-force-3d ${forD3[i][field]} pure ${forPure[i][field]}` };
        }
      }
    }
  }
  return { first: -1, compared, notFinite };
};

// ---------------------------------------------------------------------------
// The scenarios.
// ---------------------------------------------------------------------------

const results = new Map(); // group -> { scenarios, compared, notFinite, largest, failures: [] }
const record = (group, name, count, outcome) => {
  const entry = results.get(group) ?? { scenarios: 0, compared: 0, notFinite: 0, largest: 0, failures: [] };
  entry.scenarios++;
  entry.compared += outcome.compared;
  entry.notFinite += outcome.notFinite;
  entry.largest = Math.max(entry.largest, count);
  if (outcome.first >= 0) entry.failures.push({ name, ...outcome });
  results.set(group, entry);
};
const skipped = new Set();
const attempt = (group, name, count, build, scenario) => {
  if (!needs(build)) { skipped.add(group); return; }
  record(group, name, count, scenario());
};

const radiusOf = (node) => 3 + (node.index % 5);
const chargeOf = (node) => [-50, 10, 0, -5][node.index % 4];

// Each force on its own, in the variants worth telling apart.
const single = {
  'many-body': [
    ['defaults', () => [forces.manyBody()]],
    ['theta 0.5', () => [forces.manyBody({ theta: 0.5 })]],
    ['theta 0 (exact)', () => [forces.manyBody({ theta: 0 })]],
    ['distance 5..80', () => [forces.manyBody({ distanceMin: 5, distanceMax: 80 })]],
    ['per-node strength, mixed sign', () => [forces.manyBody({ strength: chargeOf, theta: 1.2 })]],
  ],
  link: [
    ['defaults', () => [forces.link()]],
    ['per-link distance and strength', () => [forces.link({ distance: (link) => link.length, strength: (link) => link.weight })]],
    ['three iterations', () => [forces.link({ distance: always(12), iterations: 3 })]],
  ],
  collide: [
    ['defaults', () => [forces.collide()]],
    ['per-node radius, strength 0.7, two iterations', () => [forces.collide({ radius: radiusOf, strength: 0.7, iterations: 2 })]],
    ['radius 8, three iterations', () => [forces.collide({ radius: always(8), iterations: 3 })]],
  ],
  center: [
    ['defaults', () => [forces.center()]],
    ['off-centre, strength 0.4', () => [forces.center({ x: 30, y: -12, z: 7, strength: 0.4 })]],
  ],
  x: [
    ['defaults', () => [forces.x()]],
    ['per-node target and strength', () => [forces.x({ target: (node) => (node.index % 3) * 40, strength: (node) => 0.05 + (node.index % 2) * 0.2 })]],
  ],
  y: [
    ['defaults', () => [forces.y()]],
    ['per-node target and strength', () => [forces.y({ target: (node) => (node.index % 4) * 25, strength: (node) => 0.3 - (node.index % 3) * 0.1 })]],
  ],
  z: [
    ['defaults', () => [forces.z()]],
    ['per-node target and strength', () => [forces.z({ target: (node) => (node.index % 5) * -15, strength: (node) => 0.1 + (node.index % 3) * 0.1 })]],
  ],
  radial: [
    ['defaults', () => [forces.radial()]],
    ['per-node radius, off-centre', () => [forces.radial({ radius: (node) => 20 + (node.index % 3) * 30, strength: always(0.25), x: 5, y: 5, z: -5 })]],
  ],
};

const sizes = quick ? [2, 10, 100] : [1, 2, 10, 100, 1000];
const layoutNames = Object.keys(layouts);

for (const [group, variants] of Object.entries(single)) {
  for (const [variant, build] of variants) {
    for (const count of sizes) {
      for (const layout of layoutNames) {
        for (const moving of [false, true]) {
          attempt(group, `${variant}, ${count} ${layout}${moving ? ' moving' : ''}`, count, build, () => {
            const random = mulberry(count * 31 + layoutNames.indexOf(layout) * 7 + (moving ? 1 : 0));
            const nodes = makeNodes(count, layout, random, { moving });
            return run({ nodes, links: ringWithChords(count), forceList: build() });
          });
        }
      }
    }
  }
}

// The usual combinations.
const combinations = [
  ['many-body + link + center', () => [forces.manyBody(), forces.link(), forces.center()]],
  ['many-body + link + center + collide', () => [forces.manyBody(), forces.link(), forces.center(), forces.collide({ radius: radiusOf })]],
  ['x + y + z + collide', () => [forces.x({ strength: always(0.2) }), forces.y({ strength: always(0.2) }), forces.z({ strength: always(0.2) }), forces.collide({ radius: radiusOf, iterations: 2 })]],
  ['link + many-body + x + y + z', () => [forces.link({ distance: (link) => link.length }), forces.manyBody({ strength: chargeOf }), forces.x(), forces.y(), forces.z()]],
  ['link + center + x + y + z + radial', () => [forces.link({ distance: (link) => link.length, strength: (link) => link.weight }), forces.center({ strength: 0.5 }), forces.x(), forces.y(), forces.z({ target: always(10) }), forces.radial({ radius: always(60) })]],
  ['all eight', () => [forces.manyBody({ distanceMax: 300 }), forces.link(), forces.collide({ radius: radiusOf, strength: 0.8 }), forces.center({ strength: 0.5 }), forces.x(), forces.y(), forces.z(), forces.radial({ radius: always(60) })]],
];
for (const [name, build] of combinations) {
  for (const count of sizes) {
    for (const layout of layoutNames) {
      for (const pinned of [0, 0.15]) {
        attempt('combinations', `${name}, ${count} ${layout}${pinned ? ' some pinned' : ''}`, count, build, () => {
          const random = mulberry(count * 131 + layoutNames.indexOf(layout) * 17 + (pinned ? 3 : 0));
          const nodes = makeNodes(count, layout, random, { pinned });
          return run({ nodes, links: ringWithChords(count), forceList: build() });
        });
      }
    }
  }
}

// Nodes that are nowhere. d3-force-3d leaves a node with any coordinate NaN
// out of the octree; centring and the radial force read a NaN y or z as zero.
{
  const nowhere = [
    ['x', (nodes) => { nodes[7].x = NaN; }],
    ['z', (nodes) => { nodes[7].z = NaN; }],
    ['y and z', (nodes) => { nodes[7].y = NaN; nodes[12].z = NaN; }],
  ];
  const kinds = [
    ['many-body', () => [forces.manyBody()]],
    ['collide', () => [forces.collide({ radius: radiusOf })]],
    ['center', () => [forces.center({ x: 3, y: 2, z: 1 })]],
    ['radial', () => [forces.radial({ x: 5, y: 5, z: -5 })]], // off the origin, where `NaN || 0` and `NaN` part ways
    ['link', () => [forces.link()]],
    ['z', () => [forces.z()]],
  ];
  for (const [which, spoil] of nowhere) {
    for (const [group, build] of kinds) {
      attempt(group, `one node at NaN ${which}`, 50, build, () => {
        const nodes = makeNodes(50, 'scattered', mulberry(5));
        spoil(nodes);
        return run({ nodes, links: ringWithChords(50), forceList: build() });
      });
    }
  }
}

// A node at NaN that exerts nothing (index 6, strength zero under chargeOf):
// it still shapes the octree, which d3-octree inserts it into at (0, 0, 0)
// and then splits by its own NaN coordinates, so every other node's numbers
// stay finite and still depend on where it went. The lattice and the heap put
// valid nodes exactly at the origin too.
for (const layout of ['scattered', 'lattice', 'heap', 'pairs', 'plane']) {
  for (const [which, spoil] of [['x', (node) => { node.x = NaN; }], ['z', (node) => { node.z = NaN; }]]) {
    for (const count of quick ? [30] : [30, 300]) {
      attempt('many-body', `per-node strength, node 6 at NaN ${which} with strength 0, ${count} ${layout}`, count, 'manyBody', () => {
        const nodes = makeNodes(count, layout, mulberry(count + 6));
        spoil(nodes[6]);
        return run({ nodes, forceList: [forces.manyBody({ strength: chargeOf })], ticks: layout === 'heap' ? 30 : ticksPerScenario });
      });
    }
  }
}

// Ten thousand nodes: many-body alone, link alone, and run.mjs's forces.
if (!quick) {
  const count = 10000;
  const nodes = makeNodes(count, 'spiral', mulberry(1));
  attempt('many-body', 'defaults, 10000 spiral', count, 'manyBody', () => run({ nodes, forceList: [forces.manyBody()] }));
  attempt('link', 'defaults, 10000 spiral', count, 'link', () => run({ nodes, links: ringWithChords(count), forceList: [forces.link()] }));
  attempt('combinations', 'many-body + link + center, 10000 spiral', count, 'manyBody', () =>
    run({ nodes, links: ringWithChords(count), forceList: [forces.manyBody(), forces.link(), forces.center()] }));
  attempt('combinations', 'many-body + link + center + collide, 10000 spiral', count, 'manyBody collide', () =>
    run({ nodes, links: ringWithChords(count), forceList: [forces.manyBody(), forces.link(), forces.center(), forces.collide({ radius: radiusOf })], ticks: 100 }));
}

// Twelve thousand nodes on one spot: a single leaf holding all of them, which
// the first tick has to take apart. Three ticks, since each is n squared.
if (!quick) {
  const count = 12000;
  const nodes = makeNodes(count, 'heap', mulberry(2));
  attempt('many-body', 'defaults, 12000 heap, 3 ticks', count, 'manyBody', () => run({ nodes, forceList: [forces.manyBody()], ticks: 3 }));
  attempt('collide', 'defaults, 12000 heap, 3 ticks', count, 'collide', () => run({ nodes, forceList: [forces.collide({ radius: radiusOf })], ticks: 3 }));
}

// Random graphs: sizes, links, forces, settings and starting points all drawn
// from one seeded generator.
{
  const random = mulberry(20261119);
  const pick = (list) => list[Math.floor(random() * list.length)];
  const graphs = quick ? 60 : 400;
  for (let graph = 0; graph < graphs; graph++) {
    const count = 1 + Math.floor(random() * 150);
    const nodes = makeNodes(count, pick(layoutNames), random, { moving: random() < 0.5, pinned: random() < 0.3 ? 0.1 : 0 });
    const links = randomLinks(count, Math.floor(random() * 2 * count), random);
    const charge = -60 * random(), radius = 1 + random() * 9, pull = random() * 0.3;
    const available = [
      ['manyBody', () => forces.manyBody({ strength: random() < 0.5 ? always(charge) : chargeOf, theta: pick([0, 0.5, 0.9, 1.5]), distanceMin: pick([1, 0.01, 4]), distanceMax: pick([Infinity, 50, 200]) })],
      ['link', () => forces.link({ distance: random() < 0.5 ? always(30) : (link) => link.length, strength: random() < 0.5 ? null : (link) => link.weight, iterations: pick([1, 1, 2, 4]) })],
      ['collide', () => forces.collide({ radius: random() < 0.5 ? always(radius) : radiusOf, strength: pick([1, 0.7, 0.3]), iterations: pick([1, 1, 2, 3]) })],
      ['center', () => forces.center({ x: pick([0, 10]), y: pick([0, -10]), z: pick([0, 5]), strength: pick([1, 0.5]) })],
      ['x', () => forces.x({ target: always(pick([0, 25])), strength: always(pull) })],
      ['y', () => forces.y({ target: always(pick([0, -25])), strength: always(pull) })],
      ['z', () => forces.z({ target: always(pick([0, 15])), strength: always(pull) })],
      ['radial', () => forces.radial({ radius: always(pick([0, 40, 120])), strength: always(pull) })],
    ];
    // Shuffle the kinds of force, then take the first few: any subset, in any order.
    const order = available.map((entry) => [random(), entry]).sort((a, b) => a[0] - b[0]).map(([, entry]) => entry);
    const chosen = order.slice(0, 1 + Math.floor(random() * order.length));
    const forceList = chosen.map(([, build]) => build());
    const settings = { alphaTarget: pick([0, 0, 0.3]), alphaDecay: pick([defaults.alphaDecay, 0.05]), velocityRetention: pick([0.6, 0.6, 0.9, 0.3]) };
    const kinds = chosen.map(([kind]) => kind).join(' ');
    attempt('random graphs', `graph ${graph} (${kinds})`, count, kinds, () => run({ nodes, links, forceList, settings }));
  }
}

// The same against forceSimulation(...).numDimensions(3) itself rather than a
// loop round its force functions.
for (const [name, build] of combinations) {
  for (const count of quick ? [10, 100] : [10, 100, 1000]) {
    for (const layout of ['spiral', 'lattice', 'heap', 'plane']) {
      attempt('against forceSimulation', `${name}, ${count} ${layout}`, count, build, () => {
        const nodes = makeNodes(count, layout, mulberry(count + layout.length), { pinned: 0.1 });
        return run({ nodes, links: ringWithChords(count), forceList: build(), reference: 'simulation' });
      });
    }
  }
}

// initializeNodes and links by id: nodes that arrive with an id and little
// else (no position, half a position, half a velocity, some pinned), linked by
// id, through the real forceSimulation(nodes, 3) and through
// ThreeD.Tick.initializeNodes + linkById. The dimension is given to the
// constructor here, because numDimensions(3) afterwards does not place the
// nodes again: they would keep a place on the flat spiral and no z.
{
  const either = (await compiled('Data.Either')).either;
  const { ordInt } = await compiled('Data.Ord');
  const { showInt } = await compiled('Data.Show');
  const PureTick = await compiled('Hylograph.Forces.ThreeD.Tick');
  const identify = (node) => node.id;
  const linkConfig = { distance: (link) => link.length, strength: PureLink.strengthFromDegree, iterations: 1 };
  const withCharge = ported.manyBody;
  for (const count of quick ? [2, 30] : [1, 2, 30, 300, 1000]) {
    const bare = () => Array.from({ length: count }, (_, i) => {
      const node = { id: 1000 + i * 3 };
      if (i % 7 === 3) node.fx = i;                                        // pinned in x only, and nowhere yet
      if (i % 13 === 4) { node.fz = -i; }                                   // pinned in z only, and nowhere yet
      if (i % 11 === 5) { node.x = -i; node.y = 2 * i; node.z = i / 2; node.vx = 1; } // a position; a third of a velocity
      if (i % 17 === 6) { node.x = i; node.y = -i; node.vx = 0; node.vy = 0; node.vz = 0.5; } // no z: placed whole
      if (i % 19 === 7) { node.x = 1; node.y = 2; node.z = 3; node.fy = 9; } // a position and a pin in y
      return node;
    });
    const links = ringWithChords(count).map((link) => ({ ...link, source: 1000 + link.source * 3, target: 1000 + link.target * 3 }));
    const forD3 = bare(), forPure = bare();
    const simulation = d3.forceSimulation(forD3, dimensions).stop()
      .force('link', d3.forceLink(links.map((link) => ({ ...link }))).id(identify).distance((link) => link.length));
    if (withCharge) simulation.force('charge', d3.forceManyBody());
    PureTick.initializeNodes(forPure)();
    const random = PureRandom.lcg();
    const built = [either((problem) => { throw new Error(problem); })((force) => force)(Pure.linkById(ordInt)(showInt)(identify)(linkConfig)(random)(forPure)(links))];
    if (withCharge) built.push(forces.manyBody().pure(random, forPure));
    let alpha = 1, compared = 0, outcome = null;
    const all = ['index', ...fields];
    for (let tick = -1; tick < ticksPerScenario && !outcome; tick++) { // tick -1 compares the nodes as initialized
      if (tick >= 0) { simulation.tick(); alpha = Pure.tick(defaults)(built)(forPure)(alpha)(); }
      for (let i = 0; i < count && !outcome; i++) for (const field of all) {
        compared++;
        if (!Object.is(forD3[i][field], forPure[i][field])) { outcome = { first: Math.max(tick, 0), compared, notFinite: 0, detail: `${tick < 0 ? 'as initialized, ' : ''}node ${i} ${field}: d3-force-3d ${forD3[i][field]} pure ${forPure[i][field]}` }; break; }
      }
    }
    record(withCharge ? 'initializeNodes + links by id' : 'initializeNodes + links by id (no many-body yet)', `${count} bare nodes`, count, outcome ?? { first: -1, compared, notFinite: 0 });
  }
  // A link to a node that is not there: d3-force-3d throws, linkById returns the same message.
  {
    const nodes = [{ id: 1 }, { id: 2 }], links = [{ source: 1, target: 2 }, { source: 2, target: 77 }];
    let thrown = null;
    try { d3.forceSimulation(nodes.map((node) => ({ ...node })), dimensions).stop().force('link', d3.forceLink(links.map((link) => ({ ...link }))).id(identify)); } catch (error) { thrown = error.message; }
    const ours = either((problem) => problem)(() => 'no error')(Pure.linkById(ordInt)(showInt)(identify)({ ...linkConfig, distance: () => 30 })(PureRandom.lcg())(nodes)(links));
    record(withCharge ? 'initializeNodes + links by id' : 'initializeNodes + links by id (no many-body yet)', 'missing node', 2, thrown === ours ? { first: -1, compared: 1, notFinite: 0 } : { first: 0, compared: 1, notFinite: 0, detail: `d3-force-3d threw "${thrown}", pure gave "${ours}"` });
  }
}

// ---------------------------------------------------------------------------
// The report.
// ---------------------------------------------------------------------------

console.log(`d3-force-3d ${d3Version} at three dimensions against ${outputDirectory}/, ${ticksPerScenario} ticks per scenario unless it says otherwise, Object.is on x, y, z, vx, vy, vz of every node after every tick\n`);
let failed = false;
for (const [group, entry] of results) {
  const verdict = entry.failures.length === 0
    ? 'identical'
    : `DIFFERS in ${entry.failures.length} scenario(s), first at tick ${Math.min(...entry.failures.map((failure) => failure.first))}`;
  // Values that are NaN or infinite in d3 too are counted apart: agreeing on NaN is weak evidence.
  console.log(`${group.padEnd(34)} ${String(entry.scenarios).padStart(4)} scenarios, up to ${String(entry.largest).padStart(5)} nodes, ${String(entry.compared).padStart(11)} values compared (${entry.notFinite} not finite): ${verdict}`);
  for (const failure of entry.failures.slice(0, 5)) console.log(`    ${failure.name}: tick ${failure.first}, ${failure.detail}`);
  if (entry.failures.length > 0) failed = true;
}
if (skipped.size > 0) console.log(`\nNot yet ported, so scenarios needing them were left out of: ${[...skipped].join(', ')}`);
process.exit(failed ? 1 : 0);

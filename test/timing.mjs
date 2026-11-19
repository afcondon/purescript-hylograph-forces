// How long does a tick take, beside d3-force?
//
//   node test/timing.mjs [--output output,output-es] [--runs 5] [--sizes 100,1000,10000] [--forces mlc]
//
// The graph, the forces and the loop are those of
// docs/kb/plans/hylograph-video-series-checks/wasm-kernel/run.mjs: a ring
// with chords laid out on d3's phyllotaxis spiral; many-body (strength -30,
// theta 0.9), link (distance 30) and centre; alpha cooled as d3 cools it and
// velocities decayed by 0.6; 300 ticks. `--forces` picks others by letter:
// m many-body, l link, c centre, k collide (radius 3 to 7).
//
// The machine this was written on is never idle, so the figure is built to
// survive that. The 300 ticks are timed in 30 blocks of ten; every
// contender is run `runs` times, taking turns; and each block counts at the
// fastest it was ever seen to run. The sum of those is the time the 300
// ticks take when nothing else gets in the way, and it is worked out the
// same way for d3-force and for the port. The plain best-of-runs total is
// printed beside it.
//
// The script also checks that every contender ends in the same place as
// d3-force, to the bit.
//
// A timing, not a benchmark suite: one machine, one graph shape, Node rather
// than a browser.
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const argument = (name, fallback) => {
  const at = process.argv.indexOf(name);
  return at < 0 ? fallback : process.argv[at + 1];
};
const outputDirectories = argument('--output', 'output').split(',');
const runs = Number(argument('--runs', 5));
const sizes = argument('--sizes', '100,1000,10000').split(',').map(Number);
const chosen = argument('--forces', 'mlc');
const blocks = 30, ticksPerBlock = 10, ticks = blocks * ticksPerBlock;

const d3 = await import(pathToFileURL(join(here, '..', 'node_modules/d3-force/src/index.js')));
const { default: d3Lcg } = await import(pathToFileURL(join(here, '..', 'node_modules/d3-force/src/lcg.js')));
const compiled = {};
for (const directory of outputDirectories) {
  const load = (module) => import(pathToFileURL(join(here, '..', directory, module, 'index.js')));
  compiled[directory] = {
    Pure: await load('Hylograph.Forces'),
    PureLink: await load('Hylograph.Forces.Link'),
    PureRandom: await load('Hylograph.Forces.Random'),
    PureManyBody: await load('Hylograph.Forces.ManyBody'),
    PureGroup: await load('Hylograph.Forces.Group'),
  };
}

const graph = (n) => {
  const links = [];
  for (let i = 0; i < n; i++) { links.push([i, (i + 1) % n]); if (i % 7 === 0) links.push([i, (i * 13 + 5) % n]); }
  return links.filter(([a, b]) => a !== b);
};
const spiral = (i) => { const r = 10 * Math.sqrt(0.5 + i), a = i * Math.PI * (3 - Math.sqrt(5)); return [r * Math.cos(a), r * Math.sin(a)]; };
const makeNodes = (n) => Array.from({ length: n }, (_, i) => { const [x, y] = spiral(i); return { index: i, x, y, vx: 0, vy: 0 }; });
const alphaDecay = 1 - Math.pow(0.001, 1 / 300);
const radiusOf = (node) => 3 + (node.index % 5);

// Each contender: given a size, the nodes and a function that runs one tick.
const viaD3 = (n) => {
  const nodes = makeNodes(n);
  const forces = [];
  if (chosen.includes('m')) forces.push(d3.forceManyBody().strength(-30).theta(0.9));
  if (chosen.includes('l')) forces.push(d3.forceLink(graph(n).map(([source, target]) => ({ source, target }))).distance(30).iterations(1));
  if (chosen.includes('c')) forces.push(d3.forceCenter(0, 0));
  if (chosen.includes('k')) forces.push(d3.forceCollide().radius(radiusOf));
  const random = d3Lcg();
  for (const force of forces) force.initialize(nodes, random);
  let alpha = 1;
  return {
    nodes,
    tick: () => {
      alpha += (0 - alpha) * alphaDecay;
      for (const force of forces) force(alpha);
      for (const node of nodes) { node.x += node.vx *= 0.6; node.y += node.vy *= 0.6; }
    },
  };
};

const viaPure = (directory) => (n) => {
  const { Pure, PureLink, PureRandom, PureManyBody, PureGroup } = compiled[directory];
  const nodes = makeNodes(n);
  const random = PureRandom.lcg();
  const forces = [];
  if (chosen.includes('m')) forces.push(Pure.manyBody({ strength: () => -30, theta: 0.9, distanceMin: 1, distanceMax: Infinity, scope: PureManyBody.everyPair, group: () => PureGroup.ungrouped })(random)(nodes));
  if (chosen.includes('l')) forces.push(Pure.link({ distance: () => 30, strength: PureLink.strengthFromDegree, iterations: 1 })(random)(nodes)(graph(n).map(([source, target]) => ({ source, target }))));
  if (chosen.includes('c')) forces.push(Pure.center({ x: 0, y: 0, strength: 1 })(nodes));
  if (chosen.includes('k')) forces.push(Pure.collide({ radius: radiusOf, strength: 1, iterations: 1, include: () => true, alphaScaled: false })(random)(nodes));
  const step = Pure.tick({ alphaTarget: 0, alphaDecay, velocityRetention: 0.6 })(forces)(nodes);
  let alpha = 1;
  return { nodes, tick: () => { alpha = step(alpha)(); } };
};

const contenders = [['d3-force', viaD3], ...outputDirectories.map((directory) => [`${directory}/`, viaPure(directory)])];
const same = (a, b) => a.every((node, i) => ['x', 'y', 'vx', 'vy'].every((field) => Object.is(node[field], b[i][field])));

console.log(`forces ${chosen}, ${ticks} ticks, ${runs} runs each taking turns; ms per tick: undisturbed (each block of ${ticksPerBlock} ticks at its fastest) / best whole run`);
for (const n of sizes) {
  const fastestBlock = contenders.map(() => new Array(blocks).fill(Infinity));
  const fastestRun = contenders.map(() => Infinity);
  let identical = true;
  for (let run = 0; run < runs; run++) {
    let reference;
    contenders.forEach(([, make], which) => {
      const { nodes, tick } = make(n);
      let whole = 0;
      for (let block = 0; block < blocks; block++) {
        const start = performance.now();
        for (let i = 0; i < ticksPerBlock; i++) tick();
        const elapsed = performance.now() - start;
        whole += elapsed;
        fastestBlock[which][block] = Math.min(fastestBlock[which][block], elapsed);
      }
      fastestRun[which] = Math.min(fastestRun[which], whole);
      if (which === 0) reference = nodes; else identical = identical && same(reference, nodes);
    });
  }
  const undisturbed = fastestBlock.map((blockTimes) => blockTimes.reduce((a, b) => a + b, 0) / ticks);
  const cells = contenders.map(([name], which) =>
    `${name} ${undisturbed[which].toFixed(3)} / ${(fastestRun[which] / ticks).toFixed(3)}` +
    (which === 0 ? '' : ` (${(undisturbed[which] / undisturbed[0]).toFixed(2)}x d3)`));
  console.log(`${String(n).padStart(6)} nodes:  ${cells.join('   ')}   final positions ${identical ? 'identical' : 'DIFFER'}`);
}

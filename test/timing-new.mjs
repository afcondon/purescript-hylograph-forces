// How long do the forces d3 does not have take, beside the Rust/WASM kernel?
//
//   node test/timing-new.mjs [--output output,output-es] [--runs 3] [--sizes 100,1000,10000]
//
// Each force alone (with the position update), and all six together, on
// run.mjs's graph: a ring with chords laid out on d3's spiral. The WASM
// kernel is the shipped build (f32, purescript-hylograph-showcases/
// wasm-force-demo/pkg), so the two do not end in the same place to the bit;
// that comparison is test/differential.mjs's, against the f64 build. Here
// only the clock is read.
//
// Measured as test/timing.mjs measures: 100 ticks in ten blocks of ten,
// every contender run `runs` times taking turns, each block counted at its
// fastest.
import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const argument = (name, fallback) => {
  const at = process.argv.indexOf(name);
  return at < 0 ? fallback : process.argv[at + 1];
};
const outputDirectories = argument('--output', 'output,output-es').split(',');
const runs = Number(argument('--runs', 3));
const sizes = argument('--sizes', '100,1000,10000').split(',').map(Number);
const blocks = 10, ticksPerBlock = 10, ticks = blocks * ticksPerBlock;

const demo = join(homedir(), 'work/afc-work/purescript-hylograph-showcases/wasm-force-demo');
const { default: init, Simulation } = await import(pathToFileURL(join(demo, 'pkg/force_kernel.js')));
await init({ module_or_path: readFileSync(join(demo, 'pkg/force_kernel_bg.wasm')) });

const compiled = {};
for (const directory of outputDirectories) {
  const load = (module) => import(pathToFileURL(join(here, '..', directory, module, 'index.js')));
  compiled[directory] = {
    Pure: await load('Hylograph.Forces.Pure'),
    Random: await load('Hylograph.Forces.Pure.Random'),
    ManyBody: await load('Hylograph.Forces.Pure.ManyBody'),
    Group: await load('Hylograph.Forces.Pure.Group'),
    Contain: await load('Hylograph.Forces.Pure.Contain'),
  };
}

const graph = (n) => {
  const links = [];
  for (let i = 0; i < n; i++) { links.push([i, (i + 1) % n]); if (i % 7 === 0) links.push([i, (i * 13 + 5) % n]); }
  return links.filter(([a, b]) => a !== b).map(([source, target]) => ({ source, target }));
};
const spiral = (i) => { const r = 10 * Math.sqrt(0.5 + i), a = i * Math.PI * (3 - Math.sqrt(5)); return [r * Math.cos(a), r * Math.sin(a)]; };
const makeNodes = (n) => Array.from({ length: n }, (_, i) => { const [x, y] = spiral(i); return { index: i, x, y, vx: 0, vy: 0 }; });
const alphaDecay = 1 - Math.pow(0.001, 1 / 300);
const NONE = 0xffffffff;

// Per-node parameters.
const slot = (node) => ({ x: ((node.index % 100) - 50) * 20, y: (Math.floor(node.index / 100) - 50) * 20 });
const halfWidth = (node) => 6 + (node.index % 3) * 3;
const groupOf = (node) => node.index % 8;
const floats = (items, get) => Float32Array.from(items, get);

// Each case: the forces for the kernel, and for the port.
const cases = {
  towards: {
    kernel: (sim, nodes) => { const id = sim.add_towards(0.1); sim.set_force_targets_xy(id, Float32Array.from(nodes.flatMap((node) => { const t = slot(node); return [t.x, t.y]; }))); },
    pure: ({ Pure }, random, nodes) => [Pure.towards({ target: slot, strength: () => 0.1 })(nodes)],
  },
  'rect collide': {
    kernel: (sim, nodes) => { const id = sim.add_rect_collide(0, 4, 1, 1); sim.set_force_half_widths(id, floats(nodes, halfWidth)); },
    pure: ({ Pure }, random, nodes) => [Pure.rectCollide({ halfWidth, halfHeight: () => 4, strength: 1, iterations: 1 })(nodes)],
  },
  'containment (disc, soft + hard)': {
    kernel: (sim, nodes) => { const id = sim.add_containment(0.5, true); sim.containment_add_circle(id, 0, 0, 8 * Math.sqrt(nodes.length)); sim.set_force_radii(id, floats(nodes, () => 3)); },
    pure: ({ Pure, Contain }, random, nodes) => { const disc = Contain.within(Contain.circle(0)(0)(8 * Math.sqrt(nodes.length))); return [Pure.contain({ region: () => disc, radius: () => 3, strength: 0.5, hard: true })(nodes)]; },
  },
  'cohesion (8 groups)': {
    kernel: (sim, nodes) => { const id = sim.add_cohesion(0.1); sim.set_force_groups(id, Uint32Array.from(nodes, groupOf)); },
    pure: ({ Pure, Group }, random, nodes) => [Pure.cohesion({ group: (node) => Group.inGroup(groupOf(node)), strength: () => 0.1 })(nodes)],
  },
  'many-body within 8 groups': {
    kernel: (sim, nodes) => { const id = sim.add_many_body(-30, 0.9, 1, Infinity); sim.set_force_groups(id, Uint32Array.from(nodes, groupOf)); sim.set_many_body_scope(id, 1); },
    pure: ({ Pure, ManyBody, Group }, random, nodes) => [Pure.manyBody({ strength: () => -30, theta: 0.9, distanceMin: 1, distanceMax: Infinity, scope: ManyBody.withinGroups, group: (node) => Group.inGroup(groupOf(node)) })(random)(nodes)],
  },
  'many-body between 8 groups': {
    kernel: (sim, nodes) => { const id = sim.add_many_body(-30, 0.9, 1, Infinity); sim.set_force_groups(id, Uint32Array.from(nodes, groupOf)); sim.set_many_body_scope(id, 2); },
    pure: ({ Pure, ManyBody, Group }, random, nodes) => [Pure.manyBody({ strength: () => -30, theta: 0.9, distanceMin: 1, distanceMax: Infinity, scope: ManyBody.betweenGroups, group: (node) => Group.inGroup(groupOf(node)) })(random)(nodes)],
  },
  directional: {
    kernel: (sim) => { sim.add_directional(30, 0, 1, 1, 1); },
    pure: ({ Pure }, random, nodes, links) => [Pure.directional({ gap: () => 30, strength: () => 1, axis: { x: 0, y: 1 }, iterations: 1 })(nodes)(links)],
  },
};
cases['all of the above'] = {
  kernel: (sim, nodes, links) => { for (const [name, one] of Object.entries(cases)) if (name !== 'all of the above') one.kernel(sim, nodes, links); },
  pure: (modules, random, nodes, links) => Object.entries(cases).filter(([name]) => name !== 'all of the above').flatMap(([, one]) => one.pure(modules, random, nodes, links)),
};

const viaKernel = (which) => (n) => {
  const nodes = makeNodes(n), links = graph(n);
  const sim = new Simulation(n);
  sim.set_positions(Float32Array.from(nodes.flatMap((node) => [node.x, node.y])));
  sim.set_links(Uint32Array.from(links.flatMap((link) => [link.source, link.target])));
  sim.enable_forces_all(false, false, false, false, false, false);
  cases[which].kernel(sim, nodes, links);
  sim.set_alpha(1 - alphaDecay);
  return { tick: () => { sim.tick(); }, done: () => sim.free() };
};
const viaPure = (directory, which) => (n) => {
  const modules = compiled[directory];
  const nodes = makeNodes(n), links = graph(n);
  const random = modules.Random.lcg();
  const forces = cases[which].pure(modules, random, nodes, links);
  const step = modules.Pure.tick({ alphaTarget: 0, alphaDecay, velocityDecay: 0.6 })(forces)(nodes);
  let alpha = 1;
  return { tick: () => { alpha = step(alpha)(); }, done: () => {} };
};

console.log(`${ticks} ticks, ${runs} runs each taking turns; ms per tick, each block of ${ticksPerBlock} ticks at its fastest`);
for (const which of Object.keys(cases)) {
  const contenders = [['wasm (f32)', viaKernel(which)], ...outputDirectories.map((directory) => [`${directory}/`, viaPure(directory, which)])];
  const cells = [];
  for (const n of sizes) {
    const fastest = contenders.map(() => new Array(blocks).fill(Infinity));
    for (let run = 0; run < runs; run++) contenders.forEach(([, make], c) => {
      const { tick, done } = make(n);
      for (let block = 0; block < blocks; block++) {
        const start = performance.now();
        for (let i = 0; i < ticksPerBlock; i++) tick();
        fastest[c][block] = Math.min(fastest[c][block], performance.now() - start);
      }
      done();
    });
    cells.push(`${String(n).padStart(6)}: ` + contenders.map(([name], c) => `${name} ${(fastest[c].reduce((a, b) => a + b, 0) / ticks).toFixed(3)}`).join('  '));
  }
  console.log(`${which}\n  ${cells.join('\n  ')}`);
}

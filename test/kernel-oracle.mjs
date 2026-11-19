// The second half of test/differential.mjs: the forces d3 does not have.
//
// There is no d3 to compare these with. The oracle is the Rust kernel
// (purescript-hylograph-showcases/wasm-force-demo/force-kernel) built at 64
// bits, which does the same arithmetic in the same order if both follow the
// rule written on each `apply_*` function there. Same inputs through both,
// and after every tick `Object.is` on every node's x, y, vx, vy, and on the
// kinetic energy, the largest displacement, the largest acceleration and
// `settled`. Every scenario also takes a
// snapshot part of the way through on both sides, runs on, restores, runs
// the same number of ticks again and requires the same end, exactly.
//
// The f64 build is not the shipped one. `npm run build:kernel` builds it from
// the crate's current source into test/kernel-f64 (rustup's toolchain and
// wasm-pack are needed); failing that, the copy the crate's own differential
// test keeps in force-kernel/target/pkg-f64 is used.
//
// How the two are lined up. The kernel applies its forces at the current
// alpha and cools afterwards; d3 (and the port) cool first. So the kernel is
// started at the once-cooled alpha, after which the two sequences are the
// same numbers. The kernel's cooling rate is always set from here, because
// its default comes from a different `pow`.
//
// What is kept out of the inputs, because the kernel deliberately departs
// from d3 there and the port follows d3: positions or velocities that are
// not finite, NaN strengths for many-body, NaN targets for x, y and radial.
import { existsSync, readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const local = join(dirname(fileURLToPath(import.meta.url)), 'kernel-f64');
// The crate's checkout: FORCE_KERNEL, or where it sits in the author's workspace.
export const kernelSource = process.env.FORCE_KERNEL || join(homedir(), 'work/afc-work/purescript-hylograph-showcases/wasm-force-demo/force-kernel');
const kept = join(kernelSource, 'target/pkg-f64');
export const kernelDirectory = existsSync(join(local, 'force_kernel.js')) ? local : kept;
export const kernelAvailable = () => existsSync(join(kernelDirectory, 'force_kernel.js'));

const NONE = 0xffffffff;
const always = (value) => () => value;
const floats = (items, get) => Float64Array.from(items, get);

export const runKernelScenarios = async ({ compiled, record, quick, mulberry, layouts, makeNodes, ringWithChords, randomLinks, defaults }) => {
  const { default: init, Simulation } = await import(pathToFileURL(join(kernelDirectory, 'force_kernel.js')));
  await init({ module_or_path: readFileSync(join(kernelDirectory, 'force_kernel_bg.wasm')) });

  const Pure = await compiled('Hylograph.Forces');
  const PureLink = await compiled('Hylograph.Forces.Link');
  const PureRandom = await compiled('Hylograph.Forces.Random');
  const PureManyBody = await compiled('Hylograph.Forces.ManyBody');
  const PureGroup = await compiled('Hylograph.Forces.Group');
  const PureContain = await compiled('Hylograph.Forces.Contain');
  const PureSnapshot = await compiled('Hylograph.Forces.Snapshot');
  const PureMeasure = await compiled('Hylograph.Forces.Measure');
  const PureRect = await compiled('Hylograph.Forces.RectCollide');
  const PureNode = await compiled('Hylograph.Forces.Node');

  const membership = (group) => (group < 0 ? PureGroup.ungrouped : PureGroup.inGroup(group));
  const groupIds = (nodes, group) => Uint32Array.from(nodes, (node) => (group(node) < 0 ? NONE : group(node)));
  const rectPairs = { all: [0, PureRect.everyPair], within: [1, PureRect.withinGroups], between: [2, PureRect.betweenGroups] };
  const scopes = { all: [0, PureManyBody.everyPair], within: [1, PureManyBody.withinGroups], between: [2, PureManyBody.betweenGroups] };

  // A region, described once and built for both sides.
  const regions = {
    rect: (x0, y0, x1, y1) => ({
      kernel: (sim, id) => sim.containment_add_rect(id, x0, y0, x1, y1),
      pure: () => PureContain.rectangle(x0)(y0)(x1)(y1),
    }),
    circle: (x, y, radius) => ({
      kernel: (sim, id) => sim.containment_add_circle(id, x, y, radius),
      pure: () => PureContain.circle(x)(y)(radius),
    }),
    polygon: (points) => ({
      kernel: (sim, id) => sim.containment_add_polygon(id, Float64Array.from(points.flat())),
      pure: () => PureContain.polygon(points.map(([x, y]) => ({ x, y }))),
    }),
  };

  // -------------------------------------------------------------------------
  // Forces: each description adds the force to the kernel and builds the
  // PureScript one. Per-node and per-link parameters are functions of the
  // node or link, evaluated here for the kernel and by the port for itself.
  // -------------------------------------------------------------------------
  const forces = {
    towards: ({ target, strength = always(0.1) }) => ({
      kernel: (sim, nodes) => {
        const id = sim.add_towards(0);
        sim.set_force_targets_xy(id, Float64Array.from(nodes.flatMap((node) => { const t = target(node); return [t.x, t.y]; })));
        sim.set_force_strengths(id, floats(nodes, strength));
      },
      pure: (random, nodes) => Pure.towards({ target, strength })(nodes),
    }),
    // Pins are read from the nodes by both: a pinned rectangle is held.
    rectCollide: ({ halfWidth, halfHeight, strength = 1, iterations = 1, include = null, pairs = 'all', group = always(-1), through = false }) => ({
      kernel: (sim, nodes) => {
        const id = sim.add_rect_collide(0, 0, strength, iterations);
        sim.set_force_half_widths(id, floats(nodes, halfWidth));
        sim.set_force_half_heights(id, floats(nodes, halfHeight));
        if (include) sim.set_collide_filter(id, Uint8Array.from(nodes, (node) => (include(node) ? 1 : 0)));
        sim.set_force_groups(id, groupIds(nodes, group));
        sim.set_rect_collide_pairs(id, rectPairs[pairs][0]);
        sim.set_rect_collide_through(id, through);
      },
      pure: (random, nodes) =>
        Pure.rectCollide({
          halfWidth, halfHeight, strength, iterations, include: include ?? always(true), pairs: rectPairs[pairs][1], group: (node) => membership(group(node)),
          heldX: PureNode.pinnedX, heldY: PureNode.pinnedY, squeezed: through ? PureRect.goesThrough : PureRect.staysPut,
        })(nodes),
    }),
    // `region(node)` is an index into `shapes`, or -1 for a node left free.
    contain: ({ shapes, region = always(0), radius = always(0), halfWidth = null, halfHeight = null, strength = 1, hard = false }) => ({
      kernel: (sim, nodes) => {
        const id = sim.add_containment(strength, hard);
        for (const shape of shapes) shape.kernel(sim, id);
        sim.set_force_regions(id, Uint32Array.from(nodes, (node) => (region(node) < 0 ? NONE : region(node))));
        sim.set_force_radii(id, floats(nodes, radius));
        if (halfWidth) sim.set_force_half_widths(id, floats(nodes, halfWidth));
        if (halfHeight) sim.set_force_half_heights(id, floats(nodes, halfHeight));
      },
      pure: (random, nodes) => {
        const built = shapes.map((shape) => shape.pure());
        const confinement = (node) => { const which = region(node); return which < 0 || which >= built.length ? PureContain.free : PureContain.within(built[which]); };
        return Pure.contain({ region: confinement, radius, halfWidth: halfWidth ?? always(0), halfHeight: halfHeight ?? always(0), strength, hard })(nodes);
      },
    }),
    cohesion: ({ group, strength = always(0.1) }) => ({
      kernel: (sim, nodes) => {
        const id = sim.add_cohesion(0);
        sim.set_force_groups(id, groupIds(nodes, group));
        sim.set_force_strengths(id, floats(nodes, strength));
      },
      pure: (random, nodes) => Pure.cohesion({ group: (node) => membership(group(node)), strength })(nodes),
    }),
    manyBody: ({ strength = always(-30), theta = 0.9, distanceMin = 1, distanceMax = Infinity, scope = 'all', group = always(-1) } = {}) => ({
      kernel: (sim, nodes) => {
        const id = sim.add_many_body(0, theta, distanceMin, distanceMax);
        sim.set_force_strengths(id, floats(nodes, strength));
        sim.set_force_groups(id, groupIds(nodes, group));
        sim.set_many_body_scope(id, scopes[scope][0]);
      },
      pure: (random, nodes) =>
        Pure.manyBody({ strength, theta, distanceMin, distanceMax, scope: scopes[scope][1], group: (node) => membership(group(node)) })(random)(nodes),
    }),
    directional: ({ gap = always(30), strength = always(1), axis = { x: 0, y: 1 }, iterations = 1, hard = false }) => ({
      kernel: (sim, nodes, links) => {
        const id = sim.add_directional(0, axis.x, axis.y, 0, iterations);
        sim.set_force_gaps(id, floats(links, gap));
        sim.set_force_strengths(id, floats(links, strength));
        sim.set_directional_hard(id, hard);
      },
      pure: (random, nodes, links) => Pure.directional({ gap, strength, axis, iterations, hard, heldX: PureNode.pinnedX, heldY: PureNode.pinnedY })(nodes)(links),
    }),
    // d3's forces, for the combinations. (The kernel has no degree-based link
    // strength, so a link strength is always given.)
    link: ({ distance = always(30), strength = always(0.5), iterations = 1 } = {}) => ({
      kernel: (sim, nodes, links) => {
        const id = sim.add_links(0, 0, iterations);
        sim.set_force_distances(id, floats(links, distance));
        sim.set_force_strengths(id, floats(links, strength));
      },
      pure: (random, nodes, links) => Pure.link({ distance, strength: PureLink.strengthPerLink(strength), iterations })(random)(nodes)(links),
    }),
    // d3's collide, and the kernel's two additions to it: a node filter, and
    // a strength that follows alpha.
    collide: ({ radius = always(1), strength = 1, iterations = 1, include = null, alphaScaled = false } = {}) => ({
      kernel: (sim, nodes) => {
        const id = sim.add_collide(0, strength, iterations);
        sim.set_force_radii(id, floats(nodes, radius));
        if (include) sim.set_collide_filter(id, Uint8Array.from(nodes, (node) => (include(node) ? 1 : 0)));
        if (alphaScaled) sim.set_collide_alpha_scaled(id, true);
      },
      pure: (random, nodes) => Pure.collide({ radius, strength, iterations, include: include ?? always(true), alphaScaled })(random)(nodes),
    }),
    center: ({ x = 0, y = 0, strength = 1 } = {}) => ({
      kernel: (sim) => { sim.add_center(x, y, strength); },
      pure: (random, nodes) => Pure.center({ x, y, strength })(nodes),
    }),
    x: ({ target = always(0), strength = always(0.1) } = {}) => ({
      kernel: (sim, nodes) => { const id = sim.add_force_x(0, 0); sim.set_force_targets(id, floats(nodes, target)); sim.set_force_strengths(id, floats(nodes, strength)); },
      pure: (random, nodes) => Pure.positionX({ target, strength })(nodes),
    }),
    y: ({ target = always(0), strength = always(0.1) } = {}) => ({
      kernel: (sim, nodes) => { const id = sim.add_force_y(0, 0); sim.set_force_targets(id, floats(nodes, target)); sim.set_force_strengths(id, floats(nodes, strength)); },
      pure: (random, nodes) => Pure.positionY({ target, strength })(nodes),
    }),
  };

  // -------------------------------------------------------------------------
  // Running one scenario.
  // -------------------------------------------------------------------------
  const run = ({ nodes, links = [], forceList, settings = defaults, ticks = 300 }) => {
    const forPure = nodes.map((node) => ({ ...node }));
    const count = nodes.length;
    const sim = new Simulation(count);
    sim.set_positions(Float64Array.from(nodes.flatMap((node) => [node.x, node.y])));
    sim.set_velocities(Float64Array.from(nodes.flatMap((node) => [node.vx, node.vy])));
    sim.set_fixed(Float64Array.from(nodes.flatMap((node) => [node.fx ?? NaN, node.fy ?? NaN])));
    sim.set_links(Uint32Array.from(links.flatMap((link) => [link.source, link.target])));
    sim.enable_forces_all(false, false, false, false, false, false);
    for (const force of forceList) force.kernel(sim, forPure, links);
    sim.set_alpha_decay(settings.alphaDecay);
    sim.set_alpha_target(settings.alphaTarget);
    sim.set_velocity_retention(settings.velocityRetention);
    let alpha = 1;
    sim.set_alpha(alpha + (settings.alphaTarget - alpha) * settings.alphaDecay);

    const random = PureRandom.lcg();
    const built = forceList.map((force) => force.pure(random, forPure, links));
    const step = Pure.tick(settings)(built)(forPure);
    let compared = 0, notFinite = 0;

    // One tick on both sides; returns a description of the first difference, or null.
    const watch = PureMeasure.watch();
    const tolerance = { step: 0.05, acceleration: 0.01 };
    const tickBoth = () => {
      PureMeasure.before(watch)(forPure)();
      sim.tick();
      alpha = step(alpha)();
      PureMeasure.after(watch)(forPure)();
      const positions = sim.get_positions(), velocities = sim.get_velocities();
      for (let i = 0; i < count; i++) {
        const theirs = [positions[i * 2], positions[i * 2 + 1], velocities[i * 2], velocities[i * 2 + 1]];
        const ours = [forPure[i].x, forPure[i].y, forPure[i].vx, forPure[i].vy];
        for (let k = 0; k < 4; k++) {
          compared++;
          if (!Number.isFinite(theirs[k])) notFinite++;
          if (!Object.is(theirs[k], ours[k])) return `node ${i} ${['x', 'y', 'vx', 'vy'][k]}: kernel ${theirs[k]} pure ${ours[k]}`;
        }
      }
      const measures = [
        ['kinetic energy', sim.kinetic_energy(), PureMeasure.kineticEnergy(forPure)()],
        ['max displacement', sim.max_displacement(), PureMeasure.maxDisplacement(watch)()],
        ['max acceleration', sim.max_acceleration(), PureMeasure.maxAcceleration(watch)()],
        ['settled', sim.settled(tolerance.step, tolerance.acceleration), PureMeasure.settled(tolerance)(watch)()],
      ];
      for (const [name, theirs, ours] of measures) {
        compared++;
        if (!Object.is(theirs, ours)) return `${name}: kernel ${theirs} pure ${ours}`;
      }
      return null;
    };

    const snapshotAfter = Math.floor(ticks / 3);
    let kernelSnapshot = null, pureSnapshot = null;
    for (let tick = 0; tick < ticks; tick++) {
      const difference = tickBoth();
      if (difference) { sim.free(); return { first: tick, compared, notFinite, detail: difference }; }
      if (tick === snapshotAfter) {
        kernelSnapshot = sim.snapshot();
        pureSnapshot = PureSnapshot.take(random)(alpha)(forPure)();
      }
    }
    // Restore, run the same number of ticks again, and require the same end.
    const end = forPure.map((node) => [node.x, node.y, node.vx, node.vy, node.fx, node.fy]);
    if (!sim.restore(kernelSnapshot)) { sim.free(); return { first: ticks, compared, notFinite, detail: 'kernel refused its own snapshot' }; }
    alpha = PureSnapshot.restore(pureSnapshot)(random)(forPure)();
    PureMeasure.forget(watch)();
    for (let tick = snapshotAfter + 1; tick < ticks; tick++) {
      const difference = tickBoth();
      if (difference) { sim.free(); return { first: tick, compared, notFinite, detail: `after restore: ${difference}` }; }
    }
    for (let i = 0; i < count; i++) {
      const again = [forPure[i].x, forPure[i].y, forPure[i].vx, forPure[i].vy, forPure[i].fx, forPure[i].fy];
      for (let k = 0; k < 6; k++) {
        compared++;
        if (!Object.is(end[i][k], again[k])) { sim.free(); return { first: ticks, compared, notFinite, detail: `restore then ${ticks - snapshotAfter - 1} ticks: node ${i} field ${k} was ${end[i][k]}, now ${again[k]}` }; }
      }
    }
    sim.free();
    // Did the forces do anything? Agreement between two things that both stood still is no evidence.
    const still = nodes.every((node, i) => Object.is(node.x, forPure[i].x) && Object.is(node.y, forPure[i].y));
    return { first: -1, compared, notFinite, still };
  };

  // -------------------------------------------------------------------------
  // Per-node and per-link parameters, chosen to be awkward.
  // -------------------------------------------------------------------------
  const groupOf = (node) => [0, 2, 5, -1][node.index % 4];            // sparse ids (1, 3 and 4 are empty), and some nodes in no group
  const twoGroups = (node) => node.index % 2;
  const noGroup = always(-1);
  const oneGroup = always(7);
  const pullOf = (node) => [0.05, 0.3, 0, 1][node.index % 4];         // includes zero
  const chargeOf = (node) => [-50, 10, 0, -5][node.index % 4];
  const slotOf = (node) => ({ x: (node.index % 5) * 20 - 40, y: Math.floor(node.index / 5) % 5 * 20 - 40 });
  const hereOf = (node) => ({ x: (node.index % 4), y: (node.index >> 2) % 4 });   // on the lattice: some nodes start exactly on target
  const someTargets = (node) => (node.index % 3 === 0 ? { x: NaN, y: 0 } : slotOf(node)); // a third of the nodes have no target
  const halfWidthOf = (node) => [6, 0, 12, 3][node.index % 4];        // some rectangles of zero width
  const halfHeightOf = (node) => [4, 0, 2, 9][node.index % 4];
  const radiusOf = (node) => 3 + (node.index % 5);
  const labelWidthOf = (node) => (node.index % 4 === 0 ? 3 : 14 + (node.index % 5) * 6);  // small squares among wide labels
  const labelHeightOf = (node) => (node.index % 4 === 0 ? 3 : 6);
  const gapOf = (link) => 10 + (link.source % 3) * 15;
  const stiffnessOf = (link) => [1, 0.4, 0][link.target % 3];

  // Regions with edges on the integer lattice, so that the lattice layouts
  // put nodes exactly on a boundary and exactly on a polygon's vertex.
  const box = regions.rect(0, 0, 3, 3);
  const flippedBox = regions.rect(60, 40, -60, -40);                  // corners given the other way round
  const sliver = regions.rect(1, -50, 1, 50);                         // zero width
  const dot = regions.rect(2, 2, 2, 2);                               // zero area
  const disc = regions.circle(0, 0, 50);
  const smallDisc = regions.circle(1, 1, 2);                          // smaller than some node radii
  const square = regions.polygon([[0, 0], [3, 0], [3, 3], [0, 3]]);   // counter-clockwise, vertices on lattice points
  const triangle = regions.polygon([[0, 3], [3, 0], [0, 0]]);         // clockwise, vertices on lattice points
  const hexagon = regions.polygon([[80, 0], [40, 69], [-40, 69], [-80, 0], [-40, -69], [40, -69]]);
  const flat = regions.polygon([[0, 0], [10, 10], [20, 20]]);         // zero area: confines nothing
  const twoPoints = regions.polygon([[0, 0], [10, 0]]);               // too few vertices: confines nothing
  const regionOf = (node) => (node.index % 5) - 1;                    // -1 (free), 0, 1, 2, 3 (3 may not exist: free)

  const single = {
    towards: [
      ['slots, mixed strengths', () => [forces.towards({ target: slotOf, strength: pullOf })]],
      ['targets on the lattice', () => [forces.towards({ target: hereOf, strength: always(0.5) })]],
      ['a third without a target', () => [forces.towards({ target: someTargets, strength: always(0.2) })]],
    ],
    'rect collide': [
      ['mixed sizes, some zero', () => [forces.rectCollide({ halfWidth: halfWidthOf, halfHeight: halfHeightOf })]],
      ['all the same, strength 0.6, three iterations', () => [forces.rectCollide({ halfWidth: always(5), halfHeight: always(5), strength: 0.6, iterations: 3 })]],
      ['all zero-area', () => [forces.rectCollide({ halfWidth: always(0), halfHeight: always(8) })]],
      ['two thirds of the rectangles collide', () => [forces.rectCollide({ halfWidth: halfWidthOf, halfHeight: halfHeightOf, include: (node) => node.index % 3 !== 0 })]],
      ['only within groups, two iterations', () => [forces.rectCollide({ halfWidth: always(7), halfHeight: always(4), iterations: 2, pairs: 'within', group: groupOf })]],
      ['only between groups, a filter too', () => [forces.rectCollide({ halfWidth: halfWidthOf, halfHeight: halfHeightOf, pairs: 'between', group: twoGroups, include: (node) => node.index % 7 !== 0 })]],
      ['within groups, nobody grouped', () => [forces.rectCollide({ halfWidth: always(7), halfHeight: always(4), pairs: 'within', group: noGroup })]],
      ['nobody collides', () => [forces.rectCollide({ halfWidth: always(7), halfHeight: always(4), include: always(false), through: true })]],
      ['squeezed goes through, mixed sizes, four iterations', () => [forces.rectCollide({ halfWidth: halfWidthOf, halfHeight: halfHeightOf, iterations: 4, through: true })]],
      ['squeezed goes through, strength 0.6, one iteration', () => [forces.rectCollide({ halfWidth: always(9), halfHeight: always(3), strength: 0.6, through: true })]],
      ['crowded wide boxes: labels among small ones, through', () => [forces.rectCollide({ halfWidth: labelWidthOf, halfHeight: labelHeightOf, iterations: 4, through: true })]],
      ['crowded wide boxes pulled to their slots, through', () => [forces.towards({ target: slotOf, strength: always(0.3) }), forces.rectCollide({ halfWidth: labelWidthOf, halfHeight: labelHeightOf, iterations: 4, through: true })]],
      ['crowded wide boxes pulled to where they are, wedged', () => [forces.towards({ target: hereOf, strength: always(0.3) }), forces.rectCollide({ halfWidth: labelWidthOf, halfHeight: labelHeightOf, iterations: 3 })]],
      ['labels against dots only (between groups), through, kept in a box', () => [forces.rectCollide({ halfWidth: labelWidthOf, halfHeight: labelHeightOf, iterations: 2, through: true, pairs: 'between', group: (node) => (node.index % 4 === 0 ? 0 : 1) }), forces.contain({ shapes: [flippedBox], halfWidth: labelWidthOf, halfHeight: labelHeightOf, hard: true })]],
    ],
    containment: [
      ['soft, box on the lattice', () => [forces.contain({ shapes: [box] })]],
      ['soft, strength 0.3, radii, flipped box', () => [forces.contain({ shapes: [flippedBox], radius: radiusOf, strength: 0.3 })]],
      ['hard only, disc, radii', () => [forces.contain({ shapes: [disc], radius: radiusOf, strength: 0, hard: true })]],
      ['soft and hard, hexagon', () => [forces.contain({ shapes: [hexagon], radius: always(2), strength: 0.5, hard: true })]],
      ['hard, square with vertices on the lattice', () => [forces.contain({ shapes: [square], hard: true })]],
      ['hard, clockwise triangle, radius 1', () => [forces.contain({ shapes: [triangle], radius: always(1), hard: true })]],
      ['rectangles in a flipped box, soft and hard', () => [forces.contain({ shapes: [flippedBox], halfWidth: halfWidthOf, halfHeight: halfHeightOf, strength: 0.5, hard: true })]],
      ['rounded rectangles in a hexagon, hard', () => [forces.contain({ shapes: [hexagon], radius: always(2), halfWidth: labelWidthOf, halfHeight: labelHeightOf, strength: 0, hard: true })]],
      ['rectangles in a disc, soft', () => [forces.contain({ shapes: [disc], radius: radiusOf, halfWidth: halfWidthOf, halfHeight: halfHeightOf, strength: 0.7 })]],
      ['rectangles, regions per node, lattice box and triangle', () => [forces.contain({ shapes: [box, triangle, smallDisc, sliver], region: regionOf, halfWidth: always(1), halfHeight: always(0.5), hard: true })]],
      ['regions per node: box, sliver, small disc; some free', () => [forces.contain({ shapes: [box, sliver, smallDisc], region: regionOf, radius: radiusOf, strength: 0.7, hard: true })]],
      ['zero-area and degenerate regions', () => [forces.contain({ shapes: [dot, flat, twoPoints, disc], region: regionOf, strength: 1, hard: true })]],
    ],
    cohesion: [
      ['sparse groups, mixed strengths', () => [forces.cohesion({ group: groupOf, strength: pullOf })]],
      ['one group', () => [forces.cohesion({ group: oneGroup, strength: always(0.2) })]],
      ['no groups at all', () => [forces.cohesion({ group: noGroup })]],
    ],
    'scoped many-body': [
      ['within, sparse groups', () => [forces.manyBody({ scope: 'within', group: groupOf })]],
      ['within, two groups, mixed charge, theta 0.5', () => [forces.manyBody({ scope: 'within', group: twoGroups, strength: chargeOf, theta: 0.5 })]],
      ['between, sparse groups', () => [forces.manyBody({ scope: 'between', group: groupOf })]],
      ['between, two groups, exact (theta 0), distance 2..120', () => [forces.manyBody({ scope: 'between', group: twoGroups, theta: 0, distanceMin: 2, distanceMax: 120 })]],
      ['between, nobody grouped', () => [forces.manyBody({ scope: 'between', group: noGroup })]],
      ['within, nobody grouped', () => [forces.manyBody({ scope: 'within', group: noGroup })]],
      ['within, one group', () => [forces.manyBody({ scope: 'within', group: oneGroup, strength: chargeOf })]],
    ],
    'yielding collide': [
      ['strength follows alpha', () => [forces.collide({ radius: radiusOf, alphaScaled: true })]],
      ['two thirds of the nodes collide', () => [forces.collide({ radius: radiusOf, include: (node) => node.index % 3 !== 0 })]],
      ['both, strength 0.7, two iterations', () => [forces.collide({ radius: always(6), strength: 0.7, iterations: 2, alphaScaled: true, include: (node) => node.index % 2 === 0 })]],
      ['nobody collides', () => [forces.collide({ radius: radiusOf, include: always(false) })]],
    ],
    directional: [
      ['downward, per-link gap and strength', () => [forces.directional({ gap: gapOf, strength: stiffnessOf })]],
      ['slanted axis, not normalised, three iterations', () => [forces.directional({ axis: { x: 3, y: -4 }, gap: always(12), iterations: 3 })]],
      ['zero axis, negative gap', () => [forces.directional({ axis: { x: 0, y: 0 }, gap: always(-5), strength: always(0.5) })]],
    ],
  };

  const combinations = [
    ['hard directional alone', () => [forces.directional({ gap: gapOf, strength: always(0), hard: true })]],
    ['hard directional, slanted axis, with the soft force', () => [forces.directional({ gap: always(25), strength: stiffnessOf, axis: { x: -2, y: 5 }, iterations: 2, hard: true })]],
    ['hard directional sideways + many-body + link', () => [forces.manyBody({ strength: always(-40) }), forces.link({ distance: gapOf }), forces.directional({ gap: always(18), strength: always(1), axis: { x: 1, y: 0 }, hard: true })]],
    ['hard directional, then hard containment of rectangles, then rect collide through', () => [forces.directional({ gap: always(12), strength: always(0.5), hard: true }), forces.contain({ shapes: [flippedBox], halfWidth: halfWidthOf, halfHeight: halfHeightOf, hard: true }), forces.rectCollide({ halfWidth: halfWidthOf, halfHeight: halfHeightOf, iterations: 2, through: true }), forces.x({ strength: always(0.05) })]],
    ['labels: towards + rect collide through + containment by half-extents', () => [forces.towards({ target: slotOf, strength: always(0.3) }), forces.rectCollide({ halfWidth: labelWidthOf, halfHeight: labelHeightOf, iterations: 4, through: true }), forces.contain({ shapes: [flippedBox], halfWidth: labelWidthOf, halfHeight: labelHeightOf, hard: true })]],
    ['towards + rect collide + containment', () => [forces.towards({ target: slotOf, strength: always(0.2) }), forces.rectCollide({ halfWidth: halfWidthOf, halfHeight: halfHeightOf, iterations: 2 }), forces.contain({ shapes: [flippedBox], radius: radiusOf, hard: true })]],
    ['many-body + link + centre + containment (hard, disc)', () => [forces.manyBody(), forces.link(), forces.center(), forces.contain({ shapes: [disc], radius: radiusOf, strength: 0.5, hard: true })]],
    ['between-group many-body + cohesion + collide', () => [forces.manyBody({ scope: 'between', group: groupOf }), forces.cohesion({ group: groupOf, strength: always(0.3) }), forces.collide({ radius: radiusOf })]],
    ['link + directional + x + rect collide', () => [forces.link({ distance: gapOf }), forces.directional({ gap: gapOf, strength: always(0.8), iterations: 2 }), forces.x({ strength: always(0.05) }), forces.rectCollide({ halfWidth: always(8), halfHeight: always(4) })]],
    ['all six new, with many-body, link, centre, x, y and collide', () => [
      forces.manyBody({ scope: 'within', group: twoGroups, strength: chargeOf }), forces.manyBody({ strength: always(-8), distanceMax: 200 }), forces.link(), forces.directional({ gap: always(20) }),
      forces.towards({ target: slotOf, strength: always(0.05) }), forces.cohesion({ group: groupOf }), forces.collide({ radius: always(2) }), forces.rectCollide({ halfWidth: halfWidthOf, halfHeight: halfHeightOf }),
      forces.center({ strength: 0.3 }), forces.x(), forces.y(), forces.contain({ shapes: [hexagon, disc], region: (node) => node.index % 2, radius: radiusOf, strength: 0.4, hard: true }),
    ]],
  ];

  const sizes = quick ? [2, 10, 100] : [1, 2, 10, 100, 1000];
  // Rectangles that stay crowded (pulled together, or kept in a small box)
  // cost a million pairs an iteration at a thousand nodes, for 300 ticks,
  // on each side. Those scenarios are run up to 100 nodes; at a thousand
  // the crowd is run once per spread-out layout, free to come apart.
  const crowdedOut = (name, count, layout) =>
    count >= 1000 && /crowded|through|labels/.test(name)
      && !(name.startsWith('crowded wide boxes: labels among small ones') && ['spiral', 'scattered', 'line'].includes(layout));
  const layoutNames = Object.keys(layouts);
  const ticks = 300;

  for (const [group, variants] of Object.entries(single)) {
    process.stderr.write(`${new Date().toISOString().slice(11, 19)} ${group}\n`);
    for (const [variant, build] of variants) {
      for (const count of sizes) {
        for (const layout of layoutNames) {
          if (crowdedOut(variant, count, layout)) continue;
          for (const moving of [false, true]) {
            const random = mulberry(count * 37 + layoutNames.indexOf(layout) * 11 + (moving ? 1 : 0));
            const nodes = makeNodes(count, layout, random, { moving, pinned: moving ? 0.1 : 0 });
            record(`${group} (kernel)`, `${variant}, ${count} ${layout}${moving ? ' moving, some pinned' : ''}`, count, run({ nodes, links: ringWithChords(count), forceList: build(), ticks }));
          }
        }
      }
    }
  }

  for (const [name, build] of combinations) {
    for (const count of sizes) {
      for (const layout of layoutNames) {
        if (crowdedOut(name, count, layout)) continue;
        for (const pinned of [0, 0.15]) {
          const random = mulberry(count * 139 + layoutNames.indexOf(layout) * 19 + (pinned ? 3 : 0));
          const nodes = makeNodes(count, layout, random, { pinned });
          record('new + d3 combined (kernel)', `${name}, ${count} ${layout}${pinned ? ' some pinned' : ''}`, count, run({ nodes, links: ringWithChords(count), forceList: build(), ticks }));
        }
      }
    }
  }

  if (!quick) {
    const count = 10000;
    const nodes = makeNodes(count, 'spiral', mulberry(3));
    for (const [name, build] of [combinations[6], combinations[9], combinations[2]]) {
      record('new + d3 combined (kernel)', `${name}, 10000 spiral, 60 ticks`, count, run({ nodes, links: ringWithChords(count), forceList: build(), ticks: 60 }));
    }
  }

  // Random graphs: any subset of the twelve kinds of force, in any order,
  // with parameters drawn from one seeded generator.
  {
    const random = mulberry(20261119);
    const pick = (list) => list[Math.floor(random() * list.length)];
    const graphs = quick ? 60 : 400;
    for (let graph = 0; graph < graphs; graph++) {
      const count = 1 + Math.floor(random() * 150);
      const nodes = makeNodes(count, pick(layoutNames), random, { moving: random() < 0.5, pinned: random() < 0.3 ? 0.1 : 0 });
      const links = randomLinks(count, Math.floor(random() * 2 * count), random);
      const grouping = pick([groupOf, twoGroups, noGroup, oneGroup]);
      const shape = pick([[box], [flippedBox], [disc], [hexagon], [square, triangle, smallDisc], [sliver, dot, flat, disc]]);
      const available = [
        () => forces.towards({ target: pick([slotOf, hereOf, someTargets]), strength: pick([pullOf, always(random() * 0.5)]) }),
        () => forces.rectCollide({ halfWidth: pick([halfWidthOf, always(random() * 10)]), halfHeight: pick([halfHeightOf, always(random() * 10)]), strength: pick([1, 0.7, 0.3]), iterations: pick([1, 1, 2, 3]) }),
        () => forces.contain({ shapes: shape, region: shape.length > 1 ? regionOf : always(0), radius: pick([always(0), radiusOf]), strength: pick([0, 0.3, 1]), hard: random() < 0.5 }),
        () => forces.cohesion({ group: grouping, strength: pick([pullOf, always(random() * 0.4)]) }),
        () => forces.manyBody({ scope: pick(['within', 'between', 'all']), group: grouping, strength: pick([chargeOf, always(-60 * random())]), theta: pick([0, 0.5, 0.9, 1.5]), distanceMin: pick([1, 0.01, 4]), distanceMax: pick([Infinity, 50, 200]) }),
        () => forces.directional({ gap: pick([gapOf, always(random() * 40 - 5)]), strength: pick([stiffnessOf, always(random())]), axis: pick([{ x: 0, y: 1 }, { x: 1, y: 0 }, { x: -2, y: 5 }, { x: 0, y: 0 }]), iterations: pick([1, 1, 2]) }),
        () => forces.link({ distance: pick([always(30), (link) => link.length]), strength: pick([always(0.5), (link) => link.weight]), iterations: pick([1, 2]) }),
        () => forces.collide({ radius: pick([always(1 + random() * 9), radiusOf]), strength: pick([1, 0.7]), iterations: pick([1, 2]), alphaScaled: random() < 0.4, include: pick([null, null, (node) => node.index % 3 !== 0]) }),
        () => forces.center({ x: pick([0, 10]), y: pick([0, -10]), strength: pick([1, 0.5]) }),
        () => forces.x({ target: always(pick([0, 25])), strength: always(random() * 0.3) }),
        () => forces.y({ target: always(pick([0, -25])), strength: always(random() * 0.3) }),
      ];
      const order = available.map((build) => [random(), build]).sort((a, b) => a[0] - b[0]).map(([, build]) => build);
      const forceList = order.slice(0, 1 + Math.floor(random() * 6)).map((build) => build());
      const settings = { alphaTarget: pick([0, 0, 0.3]), alphaDecay: pick([defaults.alphaDecay, 0.05]), velocityRetention: pick([0.6, 0.6, 0.9, 0.3]) };
      record('random graphs (kernel)', `graph ${graph}`, count, run({ nodes, links, forceList, settings, ticks }));
    }
  }

  // Random graphs again, drawing on the options added since: who collides,
  // held rectangles (the pins), the squeeze, rectangles kept in regions, and
  // hard directional constraints.
  {
    const random = mulberry(20261118);
    const pick = (list) => list[Math.floor(random() * list.length)];
    const graphs = quick ? 60 : 400;
    for (let graph = 0; graph < graphs; graph++) {
      const count = 1 + Math.floor(random() * 150);
      const nodes = makeNodes(count, pick(layoutNames), random, { moving: random() < 0.5, pinned: pick([0, 0.1, 0.3]) });
      const links = randomLinks(count, Math.floor(random() * 2 * count), random);
      const grouping = pick([groupOf, twoGroups, noGroup, oneGroup]);
      const shape = pick([[box], [flippedBox], [disc], [hexagon], [square, triangle, smallDisc], [sliver, dot, flat, disc]]);
      const sizes = pick([[halfWidthOf, halfHeightOf], [labelWidthOf, labelHeightOf], [always(random() * 12), always(random() * 6)]]);
      const available = [
        () => forces.rectCollide({ halfWidth: sizes[0], halfHeight: sizes[1], strength: pick([1, 1, 0.7]), iterations: pick([1, 2, 4]), through: random() < 0.7, pairs: pick(['all', 'all', 'within', 'between']), group: grouping, include: pick([null, null, (node) => node.index % 3 !== 0]) }),
        () => forces.contain({ shapes: shape, region: shape.length > 1 ? regionOf : always(0), radius: pick([always(0), radiusOf]), halfWidth: sizes[0], halfHeight: sizes[1], strength: pick([0, 0.3, 1]), hard: random() < 0.6 }),
        () => forces.directional({ gap: pick([gapOf, always(random() * 40 - 5)]), strength: pick([stiffnessOf, always(random()), always(0)]), axis: pick([{ x: 0, y: 1 }, { x: 1, y: 0 }, { x: -2, y: 5 }, { x: 0, y: 0 }]), iterations: pick([1, 2]), hard: random() < 0.8 }),
        () => forces.towards({ target: pick([slotOf, hereOf, someTargets]), strength: pick([pullOf, always(random() * 0.5)]) }),
        () => forces.manyBody({ strength: pick([chargeOf, always(-60 * random())]), theta: pick([0, 0.9]) }),
        () => forces.link({ distance: pick([always(30), (link) => link.length]), strength: pick([always(0.5), (link) => link.weight]) }),
        () => forces.x({ target: always(pick([0, 25])), strength: always(random() * 0.3) }),
        () => forces.y({ target: always(pick([0, -25])), strength: always(random() * 0.3) }),
      ];
      const order = available.map((build) => [random(), build]).sort((a, b) => a[0] - b[0]).map(([, build]) => build);
      const forceList = order.slice(0, 1 + Math.floor(random() * 5)).map((build) => build());
      const settings = { alphaTarget: pick([0, 0, 0.3]), alphaDecay: pick([defaults.alphaDecay, 0.05]), velocityRetention: pick([0.6, 0.4, 0.9]) };
      record('random graphs, later options (kernel)', `graph ${graph}`, count, run({ nodes, links, forceList, settings, ticks }));
    }
  }
};

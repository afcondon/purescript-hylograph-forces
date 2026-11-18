// The only JavaScript in the package: PureScript cannot assign a record field.
// The first two are all the forces use; the others serve snapshots and d3's initializeNodes.
export const writeVelocity = (node, vx, vy) => { node.vx = vx; node.vy = vy; };
export const writePosition = (node, x, y) => { node.x = x; node.y = y; };
export const writePin = (node, fx, fy) => { node.fx = fx; node.fy = fy; };
export const writeIndex = (node, index) => { node.index = index; };

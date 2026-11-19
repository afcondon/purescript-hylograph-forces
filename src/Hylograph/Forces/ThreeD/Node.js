// The three-dimensional node's writes: PureScript cannot assign a record field.
export const writeVelocity = (node, vx, vy, vz) => { node.vx = vx; node.vy = vy; node.vz = vz; };
export const writePosition = (node, x, y, z) => { node.x = x; node.y = y; node.z = z; };
export const writePin = (node, fx, fy, fz) => { node.fx = fx; node.fy = fy; node.fz = fz; };
export const writeIndex = (node, index) => { node.index = index; };

The point-on-surface evaluation happens in the function evalPatch(u, v).
It uses the cubic Bernstein basis defined in Bernstein (i, t) to form the weights Bu and Bv, does a weighted sum across the 4×4 control grid, and returns [px, py, pz].
This function is called in tessellatePatch(nU, nV) inside the nested for-loops via const pos = evalPatch(u, v);.

The control points are stored in the flat array const controlPoints = [...] at the top (commented "Control points (i-j order)"). They are interpreted as a 4×4 grid in row-major order (i = row, j = column), with each point laid out as triplets (x, y, z). Indexing is done via const idx = (r * 4 + j) * 3; inside evalPatch, i.e., (x, y, z) are at idx, idx+1, idx+2.
For visualization, they are also copied to the GPU as cpPositions = new Float32Array(controlPoints) and uploaded to cpBuffer.
class Model {
  constructor(smfText) {
    this.vertices = [];
    this.normals = [];
    this.numVertices = 0;
    this.boundingRadius = 0;

    const lines = smfText.split(/\r?\n/);
    const vertexList = [];
    const faceList = [];

    for (let line of lines) {
      line = line.trim();
      if (line.length === 0 || line[0] === "#") continue;
      const parts = line.split(/\s+/);
      if (parts[0] === "v") {
        // Vertex line: "v x y z"
        if (parts.length >= 4) {
          const x = parseFloat(parts[1]);
          const y = parseFloat(parts[2]);
          const z = parseFloat(parts[3]);
          vertexList.push(vec3(x, y, z));
          // Track max distance squared for bounding radius
          const dist2 = x*x + y*y + z*z;
          if (dist2 > this.boundingRadius) {
            this.boundingRadius = dist2;
          }
        }
      } else if (parts[0] === "f") {
        // Face line: "f i j k" (1-indexed vertex indices)
        if (parts.length >= 4) {
          const i1 = parseInt(parts[1]) - 1;
          const i2 = parseInt(parts[2]) - 1;
          const i3 = parseInt(parts[3]) - 1;
          faceList.push([i1, i2, i3]);
        }
      }
    }

    this.boundingRadius = Math.sqrt(this.boundingRadius);

    const vertexNormals = Array(vertexList.length);
    for (let i = 0; i < vertexList.length; i++) {
      vertexNormals[i] = vec3(0.0, 0.0, 0.0);
    }
    // Sum face normals into each adjacent vertex's accumulator
    for (let face of faceList) {
      const [i1, i2, i3] = face;
      const v1 = vertexList[i1];
      const v2 = vertexList[i2];
      const v3 = vertexList[i3];
      const edge1 = subtract(v2, v1);
      const edge2 = subtract(v3, v1);
      let faceNormal = cross(edge1, edge2);
      vertexNormals[i1] = add(vertexNormals[i1], faceNormal);
      vertexNormals[i2] = add(vertexNormals[i2], faceNormal);
      vertexNormals[i3] = add(vertexNormals[i3], faceNormal);
    }

    for (let i = 0; i < vertexNormals.length; i++) {
      vertexNormals[i] = normalize(vertexNormals[i]);
    }

    // Build flat arrays of interleaved vertex positions and normals for each triangle
    const flatVerts = [];
    const flatNorms = [];
    for (let face of faceList) {
      for (let idx of face) {
        const v = vertexList[idx];
        const n = vertexNormals[idx];
        flatVerts.push(v[0], v[1], v[2]);
        flatNorms.push(n[0], n[1], n[2]);
      }
    }
    this.vertices = flatVerts;
    this.normals = flatNorms;
    this.numVertices = flatVerts.length / 3;
  }
}
// Static cache to store loaded models by filename (avoid reloading)
Model.cache = {};

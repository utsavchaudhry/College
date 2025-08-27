"use strict";

class SMFModel3D {
  static shaderProgram;
  static aPositionLoc;
  static aColorLoc;
  static uModelViewLoc;
  static uProjectionLoc;

  constructor(gl, fileData) {
    const positions = [];
    const colors = [];

    const lines = fileData.split(/\r?\n/);
    const vertexList = [];
    for (let line of lines) {
      line = line.trim();
      if (line.length === 0 || line[0] === "#") {
        continue;
      }
      const parts = line.split(/\s+/);
      if (parts[0] === "v") {
        const x = parseFloat(parts[1]);
        const y = parseFloat(parts[2]);
        const z = parseFloat(parts[3]);
        vertexList.push(vec3(x, y, z));
      } else if (parts[0] === "f") {
        const indices = parts.slice(1).map(str => parseInt(str) - 1);
        for (let t = 1; t < indices.length - 1; ++t) {
          const i1 = indices[0];
          const i2 = indices[t];
          const i3 = indices[t + 1];
          const v1 = vertexList[i1];
          const v2 = vertexList[i2];
          const v3 = vertexList[i3];
          const edge1 = subtract(v2, v1);
          const edge2 = subtract(v3, v1);
          let normal = cross(edge1, edge2);
          normal = normalize(normal);
          const faceColor = vec3(
            normal[0] * 0.5 + 0.5,
            normal[1] * 0.5 + 0.5,
            normal[2] * 0.5 + 0.5
          );
          positions.push(v1, v2, v3);
          colors.push(faceColor, faceColor, faceColor);
        }
      }
    }

    this.numVertices = positions.length;

    this.vBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, this.vBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(positions), gl.STATIC_DRAW);
    gl.vertexAttribPointer(SMFModel3D.aPositionLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(SMFModel3D.aPositionLoc);

    this.cBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, this.cBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(colors), gl.STATIC_DRAW);
    gl.vertexAttribPointer(SMFModel3D.aColorLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(SMFModel3D.aColorLoc);
  }

  render() {
    gl.drawArrays(gl.TRIANGLES, 0, this.numVertices);
  }

  dispose() {
    gl.deleteBuffer(this.vBuffer);
    gl.deleteBuffer(this.cBuffer);
  }
}

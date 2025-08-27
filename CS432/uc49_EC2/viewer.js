
// -------------------------------
// Control points (i-j order)
// -------------------------------
const controlPoints = [
  0.0, 0.0, 0.0,
  2.0, 0.0, 1.5,
  4.0, 0.0, 2.9,
  6.0, 0.0, 0.0,
  0.0, 2.0, 1.1,
  2.0, 2.0, 3.9,
  4.0, 2.0, 3.1,
  6.0, 2.0, 0.7,
  0.0, 4.0, -0.5,
  2.0, 4.0, 2.6,
  4.0, 4.0, 2.4,
  6.0, 4.0, 0.4,
  0.0, 6.0, 0.3,
  2.0, 6.0, -1.1,
  4.0, 6.0, 1.3,
  6.0, 6.0, -0.2
];

// Convenient patch "center" (mean of control points on x,y; z≈0 for this data)
const PATCH_CENTER = vec3(3.0, 3.0, 0.0);

// -------------------------------
// Bernstein basis (cubic)
// -------------------------------
function bernstein(i, t) {
  const b = [
    (1 - t) ** 3,
    3 * t * (1 - t) ** 2,
    3 * t * t * (1 - t),
    t ** 3
  ];
  return b[i];
}

// -------------------------------
// Evaluate point on patch at (u, v)
// -------------------------------
function evalPatch(u, v) {
  // Interpolate in u across 4 rows
  const rowPts = new Array(4);
  const Bu = [bernstein(0, u), bernstein(1, u), bernstein(2, u), bernstein(3, u)];
  for (let r = 0; r < 4; ++r) {
    let x = 0, y = 0, z = 0;
    for (let j = 0; j < 4; ++j) {
      const idx = (r * 4 + j) * 3;
      x += Bu[j] * controlPoints[idx    ];
      y += Bu[j] * controlPoints[idx + 1];
      z += Bu[j] * controlPoints[idx + 2];
    }
    rowPts[r] = [x, y, z];
  }

  // Interpolate in v across the 4 results
  const Bv = [bernstein(0, v), bernstein(1, v), bernstein(2, v), bernstein(3, v)];
  let px = 0, py = 0, pz = 0;
  for (let i = 0; i < 4; ++i) {
    px += Bv[i] * rowPts[i][0];
    py += Bv[i] * rowPts[i][1];
    pz += Bv[i] * rowPts[i][2];
  }
  return [px, py, pz];
}

// -------------------------------
// Tessellate patch into nU × nV mesh
// Returns { vertices, normals, indices, numVertices, boundingRadius }
// -------------------------------
function tessellatePatch(nU, nV) {
  const positions = [];
  const normals   = [];
  const indices   = [];

  let maxDist2 = 0;

  // generate positions (row-major: u=i, v=j)
  for (let i = 0; i < nU; ++i) {
    const u = i / (nU - 1);
    for (let j = 0; j < nV; ++j) {
      const v = j / (nV - 1);
      const pos = evalPatch(u, v);
      positions.push(...pos);
      normals.push(0,0,0);

      // bounding radius from PATCH_CENTER
      const dx = pos[0] - PATCH_CENTER[0];
      const dy = pos[1] - PATCH_CENTER[1];
      const dz = pos[2] - PATCH_CENTER[2];
      const d2 = dx*dx + dy*dy + dz*dz;
      if (d2 > maxDist2) maxDist2 = d2;
    }
  }

  // build index grid
  const idxOf = (i,j) => i*nV + j;
  for (let i = 0; i < nU - 1; ++i) {
    for (let j = 0; j < nV - 1; ++j) {
      const i0 = idxOf(i, j);
      const i1 = idxOf(i+1, j);
      const i2 = idxOf(i+1, j+1);
      const i3 = idxOf(i, j+1);
      // two triangles
      indices.push(i0, i1, i2,  i0, i2, i3);
    }
  }

  // accumulate face normals
  for (let f = 0; f < indices.length; f += 3) {
    const i0 = indices[f], i1 = indices[f+1], i2 = indices[f+2];
    const p0 = positions.slice(i0*3, i0*3+3);
    const p1 = positions.slice(i1*3, i1*3+3);
    const p2 = positions.slice(i2*3, i2*3+3);
    const u = [p1[0]-p0[0], p1[1]-p0[1], p1[2]-p0[2]];
    const v = [p2[0]-p0[0], p2[1]-p0[1], p2[2]-p0[2]];
    const nx = u[1]*v[2] - u[2]*v[1];
    const ny = u[2]*v[0] - u[0]*v[2];
    const nz = u[0]*v[1] - u[1]*v[0];
    normals[3*i0  ] += nx; normals[3*i0+1] += ny; normals[3*i0+2] += nz;
    normals[3*i1  ] += nx; normals[3*i1+1] += ny; normals[3*i1+2] += nz;
    normals[3*i2  ] += nx; normals[3*i2+1] += ny; normals[3*i2+2] += nz;
  }

  // normalize normals
  for (let i = 0; i < normals.length; i += 3) {
    const x = normals[i], y = normals[i+1], z = normals[i+2];
    const len = Math.hypot(x,y,z) || 1.0;
    normals[i]   = x/len;
    normals[i+1] = y/len;
    normals[i+2] = z/len;
  }

  return {
    vertices: positions,
    normals: normals,
    indices: indices,
    numVertices: indices.length,
    boundingRadius: Math.sqrt(maxDist2)
  };
}

// -------------------------------
// GL setup & state
// -------------------------------
(function() {
  "use strict";

  const canvas = document.getElementById("glcanvas");
  const gl = canvas.getContext("webgl2");
  if (!gl) { alert("WebGL2 not supported by your browser"); return; }

  gl.enable(gl.DEPTH_TEST);
  gl.disable(gl.CULL_FACE);

  const program = initShaders(gl, "./vshader.glsl", "./fshader.glsl");
  gl.useProgram(program);

  // Attributes / Uniforms (names match your shaders)
  const aPositionLoc = gl.getAttribLocation(program, "aPosition");
  const aNormalLoc   = gl.getAttribLocation(program, "aNormal");

  const uModelViewMatrixLoc  = gl.getUniformLocation(program, "modelViewMatrix");
  const uProjectionMatrixLoc = gl.getUniformLocation(program, "projectionMatrix");
  const uNormalMatrixLoc     = gl.getUniformLocation(program, "uNormalMatrix"); // if present in your shader

  const uLightPosALoc = gl.getUniformLocation(program, "lightPosA"); // in camera space
  const uLightPosBLoc = gl.getUniformLocation(program, "lightPosB"); // in camera space

  const uAmbientProdLoc  = gl.getUniformLocation(program, "ambientProduct");
  const uDiffuseProdLoc  = gl.getUniformLocation(program, "diffuseProduct");
  const uSpecularProdLoc = gl.getUniformLocation(program, "specularProduct");
  const uShininessLoc    = gl.getUniformLocation(program, "shininess");

  const uShadingModeLoc  = gl.getUniformLocation(program, "uShadingMode"); // 0=gouraud, 1=phong

  const uOverlayModeLoc = gl.getUniformLocation(program, "uOverlayMode");
  const uSolidColorLoc  = gl.getUniformLocation(program, "uSolidColor");
  const uPointSizeLoc   = gl.getUniformLocation(program, "uPointSize");

  if (uOverlayModeLoc) gl.uniform1i(uOverlayModeLoc, 0);
  if (uSolidColorLoc)  gl.uniform3fv(uSolidColorLoc, new Float32Array([0,0,0]));
  if (uPointSizeLoc)   gl.uniform1f(uPointSizeLoc, 1.0);

  const cpPositions = new Float32Array(controlPoints);
  const cpBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, cpBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, cpPositions, gl.STATIC_DRAW);
  const CP_COUNT = cpPositions.length / 3;

  
  // --- Selection & Movement state for control points ---
  let selectedCP = 0;                 // index in [0, CP_COUNT-1]
  const MOVE_STEP_BASE = 0.1;         // base delta for movement (Shift: larger, Alt: smaller)

  function updateControlPointGPUBuffer() {
    // Re-upload the control point buffer after CPU-side edits
    cpPositions.set(controlPoints);
    gl.bindBuffer(gl.ARRAY_BUFFER, cpBuffer);
    gl.bufferSubData(gl.ARRAY_BUFFER, 0, cpPositions);
  }
let nU = 10, nV = 10;

  // Mesh
  let currentModel = tessellatePatch(nU, nV);

  const AXIS_LEN = Math.max(currentModel.boundingRadius * 1.2, 2.0);
  const axisPositions = new Float32Array([
    -AXIS_LEN, 0, 0,   AXIS_LEN, 0, 0,
    0, -AXIS_LEN, 0,  0, AXIS_LEN, 0,
    0, 0, -AXIS_LEN,  0, 0, AXIS_LEN
  ]);
  const axisBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, axisBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, axisPositions, gl.STATIC_DRAW);

  const positionBuffer = gl.createBuffer();
  const normalBuffer   = gl.createBuffer();
  const indexBuffer    = gl.createBuffer();

  // Camera
  let camTheta = 0.0;
  let camPhi   = 0.3;
  let camRadius = 10.0;
  let camTarget = vec3(PATCH_CENTER[0], PATCH_CENTER[1], PATCH_CENTER[2]);
  const minRadius = 1.0, maxPhi = Math.PI/2 - 0.05, minPhi = -maxPhi;

  // Projection
  let usePerspective = true;

  // Shading / Material
  let shadingMode = 0; // 0=gouraud, 1=phong
  let currentMaterialIndex = 0;

  // Lights
  // Light A: defined in OBJECT coords (cylindrical) then transformed by modelView to camera coords
  let lightA = { theta: 0.0, radius: 6.0, height: 3.0, dTheta: 0.1, dR: 0.2, dH: 0.2 };
  // Light B: defined in CAMERA coords near the eye (0, h, 0)
  let lightB = { height: 3.0, dH: 0.2 };

  camRadius = Math.max(currentModel.boundingRadius * 2.5, 6.0);

  // Upload mesh to GPU
  function updateBuffers() {
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(currentModel.vertices), gl.STATIC_DRAW);
    gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aPositionLoc);

    gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(currentModel.normals), gl.STATIC_DRAW);
    gl.vertexAttribPointer(aNormalLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aNormalLoc);

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(currentModel.indices), gl.STATIC_DRAW);
  }
  updateBuffers();

  // Materials / shading
  function applyMaterial(i) {
    const m = MATERIALS[i];
    currentMaterialIndex = i;
    gl.uniform3fv(uAmbientProdLoc,  flatten(vec3(m.ambient[0],  m.ambient[1],  m.ambient[2])));
    gl.uniform3fv(uDiffuseProdLoc,  flatten(vec3(m.diffuse[0],  m.diffuse[1],  m.diffuse[2])));
    gl.uniform3fv(uSpecularProdLoc, flatten(vec3(m.specular[0], m.specular[1], m.specular[2])));
    gl.uniform1f(uShininessLoc, m.shininess);
  }
  function applyShadingMode(modeInt) {
    shadingMode = modeInt|0;
    if (uShadingModeLoc) gl.uniform1i(uShadingModeLoc, shadingMode);
  }
  applyMaterial(0);
  applyShadingMode(0);

  // Projection upload (call each frame or when toggled)
  function projectionMatrix() {
    const aspect = canvas.width / canvas.height;
    return usePerspective
      ? perspective(60.0, aspect, 0.1, 200.0)
      : ortho(-8*aspect, 8*aspect, -8, 8, -200, 200);
  }

  // Build view matrix from orbit parameters
  function viewMatrix() {
    const eye = vec3(
      camRadius * Math.cos(camPhi) * Math.sin(camTheta) + camTarget[0],
      camRadius * Math.sin(camPhi)                       + camTarget[1],
      camRadius * Math.cos(camPhi) * Math.cos(camTheta) + camTarget[2]
    );
    return lookAt(eye, camTarget, vec3(0,1,0));
  }

  // Transform an object-space point to camera-space with modelView
  function toCameraSpace(modelView, pObj) {
    const v4 = vec4(pObj[0], pObj[1], pObj[2], 1.0);
    const r4 = mult(modelView, v4);
    return vec3(r4[0], r4[1], r4[2]);
  }

  // -------------------------------
  // Interaction
  // -------------------------------
  // Mouse
  let dragging = false, lastX = 0, lastY = 0, panMode = false;

  canvas.addEventListener('mousedown', (e) => {
    dragging = true;
    panMode = (e.button === 2) || e.ctrlKey || e.metaKey; // right button or Ctrl drag pans
    lastX = e.clientX; lastY = e.clientY;
  });
  canvas.addEventListener('mouseup', () => { dragging = false; });
  canvas.addEventListener('mouseleave', () => { dragging = false; });
  canvas.addEventListener('contextmenu', (e) => e.preventDefault());

  canvas.addEventListener('mousemove', (e) => {
    if (!dragging) return;
    const dx = (e.clientX - lastX);
    const dy = (e.clientY - lastY);
    lastX = e.clientX; lastY = e.clientY;

    if (!panMode) {
      // orbit
      camTheta += dx * 0.01;
      camPhi   = Math.min(maxPhi, Math.max(minPhi, camPhi + dy * 0.01));
    } else {
      // pan in screen plane (approximate)
      const scale = camRadius * 0.002;
      camTarget = add(camTarget, vec3(-dx * scale, dy * scale, 0.0));
    }
    // immediate feedback
    renderOnce = true;
  });

  // Wheel zoom
  canvas.addEventListener('wheel', (e) => {
    camRadius = Math.max(minRadius, camRadius + e.deltaY * 0.01 * (usePerspective ? 1.0 : 2.0));
    renderOnce = true;
    e.preventDefault();
  }, { passive: false });

  // Keyboard
  window.addEventListener('keydown', (e) => {
    switch (e.key) {
      // Tessellation
      case 'U': case 'u':
        nU = Math.max(2, nU + (e.shiftKey ? -1 : +1));
        currentModel = tessellatePatch(nU, nV);
        updateBuffers();
        renderOnce = true;
        break;
      case 'V': case 'v':
        nV = Math.max(2, nV + (e.shiftKey ? -1 : +1));
        currentModel = tessellatePatch(nU, nV);
        updateBuffers();
        renderOnce = true;
        break;

      // Projection
      case 'P': case 'p':
        usePerspective = !usePerspective;
        renderOnce = true;
        break;

      // Shading mode
      case 'G': case 'g': applyShadingMode(0); renderOnce = true; break;
      case 'H': case 'h': applyShadingMode(1); renderOnce = true; break;

      // Materials (1..3)
      case '1': applyMaterial(0); renderOnce = true; break;
      case '2': if (MATERIALS.length>1){ applyMaterial(1); renderOnce = true; } break;
      case '3': if (MATERIALS.length>2){ applyMaterial(2); renderOnce = true; } break;

      // Light A cylindrical
      case 'J': case 'j': lightA.theta -= lightA.dTheta; renderOnce = true; break;
      case 'L': case 'l': lightA.theta += lightA.dTheta; renderOnce = true; break;
      case 'I': case 'i': lightA.radius = Math.max(0.0, lightA.radius - lightA.dR); renderOnce = true; break;
      case 'K': case 'k': lightA.radius += lightA.dR; renderOnce = true; break;
      case 'O': case 'o': lightA.height -= lightA.dH; renderOnce = true; break;
      case ';':           lightA.height += lightA.dH; renderOnce = true; break;

      // Light B height near eye
      case ',': lightB.height -= lightB.dH; renderOnce = true; break;
      case '.': lightB.height += lightB.dH; renderOnce = true; break;

      // Shininess tweak
      case '[': MATERIALS[currentMaterialIndex].shininess = Math.max(1.0, MATERIALS[currentMaterialIndex].shininess - 5.0);
                applyMaterial(currentMaterialIndex); renderOnce = true; break;
      case ']': MATERIALS[currentMaterialIndex].shininess = Math.min(256.0, MATERIALS[currentMaterialIndex].shininess + 5.0);
                applyMaterial(currentMaterialIndex); renderOnce = true; break;

      // Reset (hard)
      case 'R': case 'r':
        nU = nV = 10;
        currentModel = tessellatePatch(nU, nV);
        updateBuffers();
        camTheta = 0.0; camPhi = 0.3; camRadius = Math.max(currentModel.boundingRadius*2.5, 6.0);
        camTarget = vec3(PATCH_CENTER[0], PATCH_CENTER[1], PATCH_CENTER[2]);
        usePerspective = true;
        lightA = { theta: 0.0, radius: 6.0, height: 3.0, dTheta: 0.1, dR: 0.2, dH: 0.2 };
        lightB = { height: 3.0, dH: 0.2 };
        applyMaterial(0);
        applyShadingMode(0);
        renderOnce = true;
        break;
    }
  });

  function drawOverlayPoints(buffer, count, colorRGB, pointSize) {
    gl.useProgram(program);
    gl.uniform1i(uOverlayModeLoc, 1);
    gl.uniform3fv(uSolidColorLoc, colorRGB);
    gl.uniform1f(uPointSizeLoc, pointSize);

    // Positions
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aPositionLoc);

    // Provide a constant normal so the vertex shader's normal math stays well-defined
    gl.disableVertexAttribArray(aNormalLoc);
    gl.vertexAttrib3f(aNormalLoc, 0, 0, 1);

    gl.drawArrays(gl.POINTS, 0, count);

    // Restore normal array for surface rendering later
    gl.enableVertexAttribArray(aNormalLoc);
  }

  function drawOverlaySinglePoint(buffer, index, colorRGB, pointSize) {
    gl.useProgram(program);
    gl.uniform1i(uOverlayModeLoc, 1);
    gl.uniform3fv(uSolidColorLoc, colorRGB);
    gl.uniform1f(uPointSizeLoc, pointSize);

    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aPositionLoc);

    gl.disableVertexAttribArray(aNormalLoc);
    gl.vertexAttrib3f(aNormalLoc, 0, 0, 1);

    gl.drawArrays(gl.POINTS, index, 1);
  }


  function drawOverlayLineSegment(buffer, firstVertex, colorRGB) {
    gl.useProgram(program);
    gl.uniform1i(uOverlayModeLoc, 1);
    gl.uniform3fv(uSolidColorLoc, colorRGB);
    gl.uniform1f(uPointSizeLoc, 1.0); // irrelevant for lines

    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aPositionLoc);

    gl.disableVertexAttribArray(aNormalLoc);
    gl.vertexAttrib3f(aNormalLoc, 0, 0, 1);

    gl.drawArrays(gl.LINES, firstVertex, 2);

    gl.enableVertexAttribArray(aNormalLoc);
  }


  // -------------------------------
  // Render loop
  // -------------------------------
  let renderOnce = true; // draw on demand to avoid spinning fan

  function draw() {
    if (!renderOnce) { requestAnimationFrame(draw); return; }
    renderOnce = false;

    gl.viewport(0, 0, canvas.width, canvas.height);
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    const V = viewMatrix();
    const P = projectionMatrix();
    const N = normalMatrix(V, true);

    gl.useProgram(program);
    gl.uniformMatrix4fv(uModelViewMatrixLoc,  false, flatten(V));
    gl.uniformMatrix4fv(uProjectionMatrixLoc, false, flatten(P));
    if (uNormalMatrixLoc) gl.uniformMatrix3fv(uNormalMatrixLoc, false, flatten(N));

    // Light A: object-space cylindrical around PATCH_CENTER, then to camera space
    const pObj = vec3(
      PATCH_CENTER[0] + lightA.radius * Math.cos(lightA.theta),
      PATCH_CENTER[1] + lightA.height,
      PATCH_CENTER[2] + lightA.radius * Math.sin(lightA.theta)
    );
    const pCamA = toCameraSpace(V, pObj);
    if (uLightPosALoc) gl.uniform3fv(uLightPosALoc, flatten(pCamA));

    // Light B: near eye in camera space (0,h,0)
    if (uLightPosBLoc) gl.uniform3fv(uLightPosBLoc, flatten(vec3(0.0, lightB.height, 0.0)));

    // Bind buffers & draw
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aPositionLoc);

    gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffer);
    gl.vertexAttribPointer(aNormalLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aNormalLoc);

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
    gl.drawElements(gl.TRIANGLES, currentModel.numVertices, gl.UNSIGNED_SHORT, 0);

    // 1) Control points as yellow points
    drawOverlayPoints(cpBuffer, CP_COUNT, new Float32Array([1.0, 1.0, 0.0]), 8.0);
    // Highlight the selected control point (larger size & distinct color)
    drawOverlaySinglePoint(cpBuffer, selectedCP, new Float32Array([1.0, 0.2, 0.2]), 12.0);


    // 2) Axes at object origin: X=red, Y=green, Z=blue
    //    axisPositions layout: [X0, X1, Y0, Y1, Z0, Z1] (each 2 vertices)
    drawOverlayLineSegment(axisBuffer, 0, new Float32Array([1.0, 0.0, 0.0])); // X
    drawOverlayLineSegment(axisBuffer, 2, new Float32Array([0.0, 1.0, 0.0])); // Y
    drawOverlayLineSegment(axisBuffer, 4, new Float32Array([0.0, 0.0, 1.0])); // Z

    // Return to normal (lit) mode for anything else
    gl.uniform1i(uOverlayModeLoc, 0);


    requestAnimationFrame(draw);
  }

  // Handle resize
  function resize() {
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    const w = Math.floor(canvas.clientWidth  * dpr);
    const h = Math.floor(canvas.clientHeight * dpr);
    if (canvas.width !== w || canvas.height !== h) {
      canvas.width = w; canvas.height = h;
      renderOnce = true;
    }
  }
  
  window.addEventListener('keydown', (e) => {
    const key = e.key;
    const step = (e.shiftKey ? 0.5 : (e.altKey ? 0.02 : MOVE_STEP_BASE));

    if (key === 'n' || key === 'N') {
      selectedCP = (selectedCP + 1) % CP_COUNT;
      renderOnce = true;
      e.preventDefault();
      return;
    } else if (key === 'b' || key === 'B') {
      selectedCP = (selectedCP - 1 + CP_COUNT) % CP_COUNT;
      renderOnce = true;
      e.preventDefault();
      return;
    }

    let moved = false;
    const base = selectedCP * 3;
    if (key === 'a' || key === 'A') { controlPoints[base + 0] -= step; moved = true; }
    else if (key === 'd' || key === 'D') { controlPoints[base + 0] += step; moved = true; }
    else if (key === 's' || key === 'S') { controlPoints[base + 1] -= step; moved = true; }
    else if (key === 'w' || key === 'W') { controlPoints[base + 1] += step; moved = true; }
    else if (key === 'q' || key === 'Q') { controlPoints[base + 2] -= step; moved = true; }
    else if (key === 'e' || key === 'E') { controlPoints[base + 2] += step; moved = true; }

    if (moved) {
      currentModel = tessellatePatch(nU, nV);
      updateBuffers();
      updateControlPointGPUBuffer();
      renderOnce = true;
      e.preventDefault();
    }
  });
window.addEventListener('resize', () => { resize(); });
  resize();
  draw();
})();

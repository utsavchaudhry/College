let t0 = performance.now();

(function() {
  "use strict";

  const canvas = document.getElementById("glcanvas");
  const gl = canvas.getContext("webgl2");
  if (!gl) {
    alert("WebGL2 not supported by your browser");
    return;
  }
  gl.enable(gl.DEPTH_TEST);
  gl.enable(gl.CULL_FACE);

  const program = initShaders(gl, "./vshader.glsl", "./fshader.glsl");
  gl.useProgram(program);

  // Attributes / uniforms
  const aPositionLoc     = gl.getAttribLocation(program, "aPosition");
  const aNormalLoc       = gl.getAttribLocation(program, "aNormal");

  const uModelViewMatrixLoc  = gl.getUniformLocation(program, "modelViewMatrix");
  const uProjectionMatrixLoc = gl.getUniformLocation(program, "projectionMatrix");

  const uLightPosALoc    = gl.getUniformLocation(program, "lightPosA"); // camera space
  const uLightPosBLoc    = gl.getUniformLocation(program, "lightPosB"); // camera space

  const uAmbientProdLoc  = gl.getUniformLocation(program, "ambientProduct");
  const uDiffuseProdLoc  = gl.getUniformLocation(program, "diffuseProduct");
  const uSpecularProdLoc = gl.getUniformLocation(program, "specularProduct");
  const uShininessLoc    = gl.getUniformLocation(program, "shininess");
  const uShadingModeLoc  = gl.getUniformLocation(program, "uShadingMode"); // 0=gouraud,1=phong
  const uProcScaleLoc    = gl.getUniformLocation(program, "uProcScale");

  const uTimeLoc     = gl.getUniformLocation(program, "uTime");
  const uWarpAmpLoc  = gl.getUniformLocation(program, "uWarpAmp");
  const uOctavesLoc  = gl.getUniformLocation(program, "uOctaves");

  if (uWarpAmpLoc) gl.uniform1f(uWarpAmpLoc, 0.75);
  if (uOctavesLoc) gl.uniform1i(uOctavesLoc, 6);   
  if (uTimeLoc)    gl.uniform1f(uTimeLoc, 0.0);    


  // Buffers
  const positionBuffer = gl.createBuffer();
  const normalBuffer   = gl.createBuffer();

  // App state
  const camera    = new Camera();
  const lighting  = new Lighting();
  let currentModel = null;
  let currentModelName = null;

  // Always Phong
  function applyShadingModePhong() {
    if (uShadingModeLoc) gl.uniform1i(uShadingModeLoc, 1);
  }

  // Single white material; treat diffuseProduct as LIGHT diffuse (white).
  function applyMaterialWhiteSpecular() {
    const m = MATERIALS[0];
    gl.uniform3fv(uAmbientProdLoc,  flatten(vec3(m.ambient[0],  m.ambient[1],  m.ambient[2])));
    gl.uniform3fv(uDiffuseProdLoc,  new Float32Array([1, 1, 1])); // light diffuse = white
    gl.uniform3fv(uSpecularProdLoc, flatten(vec3(m.specular[0], m.specular[1], m.specular[2])));
    gl.uniform1f(uShininessLoc, m.shininess);
  }

  function updateBuffers() {
    if (!currentModel) return;

    // positions
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(currentModel.vertices), gl.STATIC_DRAW);
    gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aPositionLoc);

    // normals
    gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(currentModel.normals), gl.STATIC_DRAW);
    gl.vertexAttribPointer(aNormalLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aNormalLoc);
  }

  function loadModel(filename) {
    function afterLoad(model) {
      currentModel = model;
      currentModelName = filename;
      camera.setDistance(currentModel.boundingRadius * 3);
      camera.setTarget(vec3(0.0, 0.0, 0.0));  // assume model centered near origin
      updateBuffers();

      // Set checker scale to about ~10 cells across the model diameter
      if (uProcScaleLoc) {
        const cellsAcross = 10.0;
        const scale = (currentModel.boundingRadius > 0.0)
          ? (cellsAcross / currentModel.boundingRadius)
          : 1.0;
        gl.uniform1f(uProcScaleLoc, scale);
      }
    }

    if (Model.cache[filename]) {
      afterLoad(Model.cache[filename]);
    } else {
      const request = new XMLHttpRequest();
      request.open("GET", filename, true);
      request.onreadystatechange = function() {
        if (request.readyState === 4 && request.status === 200) {
          const fileText = request.responseText;
          const model = new Model(fileText);
          Model.cache[filename] = model;
          afterLoad(model);
        }
      };
      request.send();
    }
  }

  // --- UI (robust guards) ---
  const modelSelect = document.getElementById("modelSelect");
  if (modelSelect) {
    modelSelect.addEventListener("change", (e) => {
      loadModel(e.target.value);
    });
  }

  // Light A sliders
  const lightAAngle  = document.getElementById("lightAAngle");
  const lightARadius = document.getElementById("lightARadius");
  const lightAHeight = document.getElementById("lightAHeight");
  if (lightAAngle)  lightAAngle.addEventListener("input", e => { lighting.lightA.angle  = parseFloat(e.target.value) * Math.PI/180.0; });
  if (lightARadius) lightARadius.addEventListener("input", e => { lighting.lightA.radius = parseFloat(e.target.value); });
  if (lightAHeight) lightAHeight.addEventListener("input", e => { lighting.lightA.height = parseFloat(e.target.value); });

  // Light B slider
  const lightBHeight = document.getElementById("lightBHeight");
  if (lightBHeight) {
    lightBHeight.addEventListener("input", e => {
      lighting.lightB.height = parseFloat(e.target.value);
      gl.uniform3fv(uLightPosBLoc, flatten(vec3(0.0, lighting.lightB.height, 0.0)));
    });
  }

  // Keyboard: orbit + zoom only
  window.addEventListener("keydown", function(e) {
    const key = e.key;
    let handled = false;
    if (key === "ArrowLeft")  { camera.incrementTheta(-5); handled = true; }
    if (key === "ArrowRight") { camera.incrementTheta( 5); handled = true; }
    if (key === "ArrowUp")    { camera.incrementPhi( -5); handled = true; }
    if (key === "ArrowDown")  { camera.incrementPhi(  5); handled = true; }
    if (key === "z" || key === "Z") { camera.zoom(0.9); handled = true; }
    if (key === "x" || key === "X") { camera.zoom(1.1); handled = true; }
    if (handled) e.preventDefault();
  });

  // Static state
  applyShadingModePhong();
  applyMaterialWhiteSpecular();

  // Projection (once; if you resize canvas elsewhere, update accordingly)
  const aspect = canvas.width / canvas.height;
  const projMatrix = perspective(60.0, aspect, 0.1, 100.0);
  gl.uniformMatrix4fv(uProjectionMatrixLoc, false, flatten(projMatrix));

  // Light B in camera space (fixed near eye)
  gl.uniform3fv(uLightPosBLoc, flatten(vec3(0.0, lighting.lightB.height, 0.0)));

  // Initial model
  const defaultModel = (modelSelect && modelSelect.value) ? modelSelect.value : "bunny.smf";
  loadModel(defaultModel);

  // Render loop
  function render() {
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    if (currentModel) {
      const mvMatrix = camera.getViewMatrix();
      gl.uniformMatrix4fv(uModelViewMatrixLoc, false, flatten(mvMatrix));

      // Light A → camera space
      const lightA_cam = lighting.getLightAInCamera(camera);
      gl.uniform3fv(uLightPosALoc, flatten(lightA_cam));

      const t = (performance.now() - t0) * 0.001;  // seconds
      if (uTimeLoc) gl.uniform1f(uTimeLoc, t);

      gl.drawArrays(gl.TRIANGLES, 0, currentModel.numVertices);
    }
    requestAnimationFrame(render);
  }
  render();
})();

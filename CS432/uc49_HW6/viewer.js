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

  const aPositionLoc     = gl.getAttribLocation(program, "aPosition");
  const aNormalLoc       = gl.getAttribLocation(program, "aNormal");
  const uModelViewMatrixLoc  = gl.getUniformLocation(program, "modelViewMatrix");
  const uProjectionMatrixLoc = gl.getUniformLocation(program, "projectionMatrix");
  const uLightPosALoc    = gl.getUniformLocation(program, "lightPosA");
  const uLightPosBLoc    = gl.getUniformLocation(program, "lightPosB");
  const uAmbientProdLoc  = gl.getUniformLocation(program, "ambientProduct");
  const uDiffuseProdLoc  = gl.getUniformLocation(program, "diffuseProduct");
  const uSpecularProdLoc = gl.getUniformLocation(program, "specularProduct");
  const uShininessLoc    = gl.getUniformLocation(program, "shininess");
  const uShadingModeLoc  = gl.getUniformLocation(program, "uShadingMode");

  const positionBuffer = gl.createBuffer();
  const normalBuffer   = gl.createBuffer();

  const camera   = new Camera();
  const lighting = new Lighting();
  const materials = MATERIALS;
  let currentMaterialIndex = 0;
  let currentShadingMode = "gouraud";

  let currentModel = null;
  let currentModelName = null;

  function loadModel(filename) {
    if (Model.cache[filename]) {
      currentModel = Model.cache[filename];
      camera.setDistance(currentModel.boundingRadius * 3);
      updateBuffers();
    } else {
      const request = new XMLHttpRequest();
      request.open("GET", filename, true);
      request.onreadystatechange = function() {
        if (request.readyState === 4 && request.status === 200) {
          const fileText = request.responseText;
          currentModel = new Model(fileText);
          Model.cache[filename] = currentModel;
          camera.setDistance(currentModel.boundingRadius * 3);
          updateBuffers();
        }
      };
      request.send();
    }
    currentModelName = filename;
  }

  function updateBuffers() {
    if (!currentModel) return;
    // Upload vertex positions
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(currentModel.vertices), gl.STATIC_DRAW);
    gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aPositionLoc);
    // Upload vertex normals
    gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(currentModel.normals), gl.STATIC_DRAW);
    gl.vertexAttribPointer(aNormalLoc, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(aNormalLoc);
  }

  function applyMaterial(index) {
    const mat = materials[index];
    currentMaterialIndex = index;
    gl.uniform3fv(uAmbientProdLoc,  flatten(vec3(mat.ambient[0],  mat.ambient[1],  mat.ambient[2])));
    gl.uniform3fv(uDiffuseProdLoc,  flatten(vec3(mat.diffuse[0],  mat.diffuse[1],  mat.diffuse[2])));
    gl.uniform3fv(uSpecularProdLoc, flatten(vec3(mat.specular[0], mat.specular[1], mat.specular[2])));
    gl.uniform1f(uShininessLoc, mat.shininess);
  }

  // (0 = Gouraud, 1 = Phong) via uniform
  function applyShadingMode(mode) {
    currentShadingMode = mode;
    const modeVal = (mode === "phong") ? 1 : 0;
    gl.uniform1i(uShadingModeLoc, modeVal);
  }

  // UI event handlers:
  document.getElementById("modelSelect").addEventListener("change", function(e) {
    loadModel(e.target.value);
  });
  document.getElementById("shadingSelect").addEventListener("change", function(e) {
    applyShadingMode(e.target.value);
  });
  document.getElementById("materialSelect").addEventListener("change", function(e) {
    applyMaterial(parseInt(e.target.value));
  });
  // Light A slider controls
  document.getElementById("lightAAngle").addEventListener("input", function(e) {
    lighting.lightA.angle = parseFloat(e.target.value) * Math.PI / 180.0;
  });
  document.getElementById("lightARadius").addEventListener("input", function(e) {
    lighting.lightA.radius = parseFloat(e.target.value);
  });
  document.getElementById("lightAHeight").addEventListener("input", function(e) {
    lighting.lightA.height = parseFloat(e.target.value);
  });
  // Light B slider control
  document.getElementById("lightBHeight").addEventListener("input", function(e) {
    lighting.lightB.height = parseFloat(e.target.value);
    // Update Light B uniform immediately (Light B is fixed in camera coords)
    gl.uniform3fv(uLightPosBLoc, flatten(vec3(0.0, lighting.lightB.height, 0.0)));
  });

  window.addEventListener("keydown", function(e) {
    const key = e.key;
    let handled = false;
    if (key === "ArrowLeft") {
      camera.incrementTheta(-5);
      handled = true;
    } else if (key === "ArrowRight") {
      camera.incrementTheta(5);
      handled = true;
    } else if (key === "ArrowUp") {
      camera.incrementPhi(-5);
      handled = true;
    } else if (key === "ArrowDown") {
      camera.incrementPhi(5);
      handled = true;
    } else if (key === "z" || key === "Z") {
      camera.zoom(0.9);
      handled = true;
    } else if (key === "x" || key === "X") {
      camera.zoom(1.1);
      handled = true;
    }
    if (handled) {
      e.preventDefault();
    }
  });

  applyShadingMode("gouraud");
  applyMaterial(0);
  gl.uniform3fv(uLightPosBLoc, flatten(vec3(0.0, lighting.lightB.height, 0.0)));
  loadModel(document.getElementById("modelSelect").value);

  const aspect = canvas.width / canvas.height;
  const projMatrix = perspective(60.0, aspect, 0.1, 100.0);
  gl.uniformMatrix4fv(uProjectionMatrixLoc, false, flatten(projMatrix));

  function render() {
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    if (currentModel) {
      // Update camera view matrix and send to shader
      const mvMatrix = camera.getViewMatrix();
      gl.uniformMatrix4fv(uModelViewMatrixLoc, false, flatten(mvMatrix));
      // Compute Light A position in camera coordinates and update uniform
      const lightA_cam = lighting.getLightAInCamera(camera);
      gl.uniform3fv(uLightPosALoc, flatten(lightA_cam));
      // Draw the model (triangle list)
      gl.drawArrays(gl.TRIANGLES, 0, currentModel.numVertices);
    }
    requestAnimationFrame(render);
  }
  render();
})();

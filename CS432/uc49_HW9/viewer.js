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

  const uColorIndexLoc = gl.getUniformLocation(program, "uColorIndex");
  gl.uniform1i(uColorIndexLoc, 0);  // default to 0 (normal rendering)


  // Create off-screen framebuffer for picking
  const pickFramebuffer = gl.createFramebuffer();
  gl.bindFramebuffer(gl.FRAMEBUFFER, pickFramebuffer);

  // Create a texture to store the color buffer
  const pickTexture = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, pickTexture);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, canvas.width, canvas.height, 0, 
                gl.RGBA, gl.UNSIGNED_BYTE, null);
  // Set texture filtering (nearest for accurate color picking, no interpolation)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

  // Attach texture as the FBO's color buffer
  gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, 
                          gl.TEXTURE_2D, pickTexture, 0);

  // Create and attach a renderbuffer for depth buffering
  const depthBuffer = gl.createRenderbuffer();
  gl.bindRenderbuffer(gl.RENDERBUFFER, depthBuffer);
  gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, canvas.width, canvas.height);
  gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, depthBuffer);

  // Unbind to use default framebuffer for normal rendering
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);


  const models = [];
  const positionBuffers = [];
  const normalBuffers = [];

  function layoutObjectsOnLine() {
    // choose a gap based on the largest radius
    const maxR = Math.max(...models.map(m => m.boundingRadius || 1.0));
    const gap  = 2.2 * maxR;             // > 2*R ensures separation
    objectPositions = [
      vec3(-gap, 0.0, 0.0),
      vec3( 0.0, 0.0, 0.0),
      vec3( gap, 0.0, 0.0)
    ];
  }

  function fitCameraToObjects(pad = 1.25) {  // pad > 1 pulls the camera "slightly backward"
    if (models.length !== 3) return;

    // Compute scene AABB using centers and bounding radii
    let minX= Infinity, maxX=-Infinity, minY= Infinity, maxY=-Infinity, minZ= Infinity, maxZ=-Infinity;
    for (let i = 0; i < 3; ++i) {
      const r = models[i].boundingRadius || 1.0;
      const p = objectPositions[i];
      minX = Math.min(minX, p[0] - r); maxX = Math.max(maxX, p[0] + r);
      minY = Math.min(minY, p[1] - r); maxY = Math.max(maxY, p[1] + r);
      minZ = Math.min(minZ, p[2] - r); maxZ = Math.max(maxZ, p[2] + r);
    }

    // Center of the trio
    const cx = 0.5 * (minX + maxX);
    const cy = 0.5 * (minY + maxY);
    const cz = 0.5 * (minZ + maxZ);
    camera.at = vec3(cx, cy, cz);    // look-at the trio’s center

    // Required camera distance from FOV (use whichever is tighter: horizontal/vertical)
    const vfov = 60.0 * Math.PI / 180.0;
    const aspect = canvas.width / canvas.height;
    const hfov = 2.0 * Math.atan(Math.tan(vfov * 0.5) * aspect);

    const halfW = 0.5 * (maxX - minX);
    const halfH = 0.5 * (maxY - minY);

    const distH = halfW / Math.tan(hfov * 0.5);
    const distV = halfH / Math.tan(vfov * 0.5);
    const needed = Math.max(distH, distV);

    camera.setDistance(needed * pad); // “slightly backward” via padding (e.g., 1.25)
  }



  let objectPositions = [
    vec3(-2.0, 0.0, 0.0),
    vec3( 0.0, 0.0, 0.0),
    vec3( 2.0, 0.0, 0.0)
  ];

  const modelFiles = ["bound-lo-sphere.smf", "bunny_5k.smf", "teapot.smf"];

  modelFiles.forEach(filename => {
      const request = new XMLHttpRequest();
      request.open("GET", filename, true);
      request.onreadystatechange = function() {
        if (request.readyState === 4 && request.status === 200) {
          const fileText = request.responseText;
          // Parse the model text into a Model object
          let model = new Model(fileText);
          Model.cache[filename] = model;
          models.push(model);

          // Create and fill GPU buffers for this model’s geometry
          const posBuf = gl.createBuffer();
          gl.bindBuffer(gl.ARRAY_BUFFER, posBuf);
          gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(model.vertices), gl.STATIC_DRAW);
          positionBuffers.push(posBuf);

          const normBuf = gl.createBuffer();
          gl.bindBuffer(gl.ARRAY_BUFFER, normBuf);
          gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(model.normals), gl.STATIC_DRAW);
          normalBuffers.push(normBuf);

          if (models.length === modelFiles.length) {
            layoutObjectsOnLine();   
            fitCameraToObjects(1.25);
          }

        }
      };
      request.send();
  });


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

  function T(v) {
    return translate(v[0], v[1], v[2]);
  }


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
      } else if (key === "r" || key === "R") {
          camera.reset();                  // **Reset the view** to original state
          handled = true;
      }
      if (handled) {
          e.preventDefault();
      }
  });

  canvas.addEventListener("mousedown", function(event) {
      // 1. Render the scene to the off-screen framebuffer with flat colors
      gl.bindFramebuffer(gl.FRAMEBUFFER, pickFramebuffer);
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      // For each object, draw with a unique color (using uColorIndex = object ID)
      models.forEach((model, i) => {
          gl.uniform1i(uColorIndexLoc, i+1);  // set color index (1,2,3 for our objects)
          // Use the same vertex data, but now shader will output flat color
          const mv = mult(camera.getViewMatrix(), T(objectPositions[i]));
          gl.uniformMatrix4fv(uModelViewMatrixLoc, false, flatten(mv));
          gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffers[i]);
          gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
          gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffers[i]);
          gl.vertexAttribPointer(aNormalLoc, 3, gl.FLOAT, false, 0, 0);
          gl.drawArrays(gl.TRIANGLES, 0, model.numVertices);
      });

      // 2. Read the pixel color at the click position from the off-screen buffer
      const x = event.offsetX; 
      const y = canvas.height - event.offsetY - 1;  // flip Y coordinate for WebGL origin
      const pixel = new Uint8Array(4);
      gl.readPixels(x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, pixel);
      gl.bindFramebuffer(gl.FRAMEBUFFER, null);  // return to onscreen framebuffer

      // 3. Determine which object was clicked based on the color
      let pickedObjectIndex = null;
      if (pixel[0] === 255) { 
          pickedObjectIndex = 0;    // Red = object 0
      } else if (pixel[1] === 255) { 
          pickedObjectIndex = 1;    // Green = object 1
      } else if (pixel[2] === 255) { 
          pickedObjectIndex = 2;    // Blue = object 2
      }
      // (If pixel is [0,0,0], then no object was under the cursor – it was background.)

      // 4. If an object was selected, change its diffuse color to a new random color
      if (pickedObjectIndex !== null) {
          // Generate a random RGB color
          const newColor = vec3(Math.random(), Math.random(), Math.random());
          const matIndex = objectMaterialIndices[pickedObjectIndex];
          // Update the material’s diffuse (and ambient) component for that object
          materials[matIndex].diffuse = vec4(newColor[0], newColor[1], newColor[2], 1.0);
          materials[matIndex].ambient = vec4(0.2*newColor[0], 0.2*newColor[1], 0.2*newColor[2], 1.0);
          // Re-apply material to update shader uniforms
          applyMaterial(matIndex);
      }

      // 5. Re-render the scene to the screen to show updated color
      gl.uniform1i(uColorIndexLoc, 0);  // ensure normal shading mode
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      models.forEach((model, i) => {
          const mv = mult(camera.getViewMatrix(), T(objectPositions[i]));
          gl.uniformMatrix4fv(uModelViewMatrixLoc, false, flatten(mv));
          applyMaterial(objectMaterialIndices[i]);
          gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffers[i]);
          gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
          gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffers[i]);
          gl.vertexAttribPointer(aNormalLoc, 3, gl.FLOAT, false, 0, 0);
          gl.drawArrays(gl.TRIANGLES, 0, model.numVertices);
      });
  });



  applyShadingMode("gouraud");
  applyMaterial(0);

  const objectMaterialIndices = [0, 1, 2];
  applyMaterial(objectMaterialIndices[0]);


  gl.uniform3fv(uLightPosBLoc, flatten(vec3(0.0, lighting.lightB.height, 0.0)));

  const aspect = canvas.width / canvas.height;
  const projMatrix = perspective(60.0, aspect, 0.1, 100.0);
  gl.uniformMatrix4fv(uProjectionMatrixLoc, false, flatten(projMatrix));

  function render() {
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    const viewMatrix = camera.getViewMatrix();
    // Update Light A position in camera coords (Light B is already fixed to camera)
    const lightA_cam = lighting.getLightAInCamera(camera);
    gl.uniform3fv(uLightPosALoc, flatten(lightA_cam));
    gl.uniform3fv(uLightPosBLoc, flatten(vec3(0.0, lighting.lightB.height, 0.0)));

    // Draw each object in the scene:
    models.forEach((model, i) => {
        // Compute model-view matrix for this object
        const modelMatrix = T(objectPositions[i]);
        const mvMatrix = mult(viewMatrix, modelMatrix);
        gl.uniformMatrix4fv(uModelViewMatrixLoc, false, flatten(mvMatrix));

        // Apply the object's material (set ambient, diffuse, specular uniforms)
        applyMaterial(objectMaterialIndices[i]);

        // Bind this object's vertex buffers and draw
        gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffers[i]);
        gl.vertexAttribPointer(aPositionLoc, 3, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(aPositionLoc);

        gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffers[i]);
        gl.vertexAttribPointer(aNormalLoc, 3, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(aNormalLoc);

        gl.drawArrays(gl.TRIANGLES, 0, model.numVertices);
    });

    requestAnimationFrame(render);
  }


  render();
})();

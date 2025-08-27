"use strict";

let gl, canvas;
let currentModel = null;
let camera = null;

const modelFiles = ["bunny_200.smf", "bunny_5k.smf", "Small_Supertoroid.smf", "cow.smf"];

window.onload = function() {
  canvas = document.getElementById("gl-canvas");
  gl = canvas.getContext("webgl2");
  if (!gl) {
    alert("WebGL 2.0 isn't available");
    return;
  }

  gl.viewport(0, 0, canvas.width, canvas.height);
  gl.clearColor(0.9, 0.9, 0.9, 1.0);
  gl.enable(gl.DEPTH_TEST);
  gl.enable(gl.CULL_FACE);
  gl.cullFace(gl.BACK);

  SMFModel3D.shaderProgram = initShaders(gl, "./vshader.glsl", "./fshader.glsl");
  gl.useProgram(SMFModel3D.shaderProgram);
  SMFModel3D.aPositionLoc = gl.getAttribLocation(SMFModel3D.shaderProgram, "aPosition");
  SMFModel3D.aColorLoc    = gl.getAttribLocation(SMFModel3D.shaderProgram, "aColor");
  SMFModel3D.uModelViewLoc  = gl.getUniformLocation(SMFModel3D.shaderProgram, "uModelView");
  SMFModel3D.uProjectionLoc = gl.getUniformLocation(SMFModel3D.shaderProgram, "uProjection");

  camera = new Camera();

  const select = document.getElementById("modelSelect");
  modelFiles.forEach(file => {
    const option = document.createElement("option");
    option.value = file;
    option.text  = file;
    select.appendChild(option);
  });
  select.selectedIndex = 0;
  loadModel(select.value);
  select.onchange = function() {
    loadModel(select.value);
  };

  window.addEventListener("keydown", event => {
    const key = event.key;
    switch (key) {
      case "ArrowUp":
        camera.incrementHeight(0.1);
        break;
      case "ArrowDown":
        camera.incrementHeight(-0.1);
        break;
      case "ArrowLeft":
        camera.incrementAngle(-5);
        break;
      case "ArrowRight":
        camera.incrementAngle(5);
        break;
      case "i":
      case "I":
        camera.incrementRadius(-0.1);
        break;
      case "o":
      case "O":
        camera.incrementRadius(0.1);
        break;
      case "p":
      case "P":
        camera.toggleProjection();
        break;
      case "r":
      case "R":
        camera.reset();
        break;
      default:
        return;
    }
    render();
  });
};

function loadModel(filename) {
  if (currentModel) {
    currentModel.dispose();
    currentModel = null;
  }
  fetch(filename)
    .then(response => {
      if (!response.ok) throw new Error(`Failed to load ${filename}`);
      return response.text();
    })
    .then(data => {
      currentModel = new SMFModel3D(gl, data);
      render();
    })
    .catch(err => {
      console.error(err);
      alert("Error loading model file.");
    });
}

function render() {
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
  if (!currentModel) return;
  const modelViewMat = camera.getViewMatrix();
  const projectionMat = camera.getProjectionMatrix(canvas.width / canvas.height);
  gl.uniformMatrix4fv(SMFModel3D.uModelViewLoc, false, flatten(modelViewMat));
  gl.uniformMatrix4fv(SMFModel3D.uProjectionLoc, false, flatten(projectionMat));
  currentModel.render();
}

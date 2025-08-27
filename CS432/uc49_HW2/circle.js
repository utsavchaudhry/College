
function getCircleCoordinates(radius, segments = 360, offsetX = 0, offsetY = 0) {
  const coords = [];
  for (let i = 0; i < segments; i++) {
    const angle = (i / segments) * 2 * Math.PI;
    coords.push(vec2((radius * Math.cos(angle)) + offsetX, (radius * Math.sin(angle)) + offsetY));
  }
  return coords;
}

function getCircleRedShading(segments = 360) {
  const colors = [];
  for (let i = 0; i < segments; i++) {
    const angle = (i / segments) * 2 * Math.PI;
    colors.push(vec3((1 + Math.cos(angle)) / 2, 0.0, 0.0));
  }
  return colors;
}

class Circle2D {
  static vertexPositions = getCircleCoordinates(0.15, 360, 0.4, 0.35);
  static colors = getCircleRedShading(360);
  static shaderProgram = -1;
  static positionBuffer = -1;
  static colorBuffer = -1;
  static aPositionShader = -1;
  static aColorShader = -1;

  static initialize() {
    Circle2D.shaderProgram = initShaders(gl, "./vshader.glsl", "./fshader.glsl");

    Circle2D.positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Circle2D.positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(Circle2D.vertexPositions), gl.STATIC_DRAW);

	Circle2D.aPositionShader = gl.getAttribLocation(Circle2D.shaderProgram, "aPosition");

	Circle2D.colorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Circle2D.colorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(Circle2D.colors), gl.STATIC_DRAW);

	Circle2D.aColorShader = gl.getAttribLocation(Circle2D.shaderProgram, "aColor");
  }

  constructor() {
    if (Circle2D.shaderProgram === -1) Circle2D.initialize();
  }

  draw() {
    gl.useProgram(Circle2D.shaderProgram);

	gl.bindBuffer(gl.ARRAY_BUFFER, Circle2D.colorBuffer);
    gl.vertexAttribPointer(Circle2D.aColorShader, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Circle2D.aColorShader);

    gl.bindBuffer(gl.ARRAY_BUFFER, Circle2D.positionBuffer);
    gl.vertexAttribPointer(Circle2D.aPositionShader, 2, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Circle2D.aPositionShader);
    // Draw all 360 vertices of the circle
    gl.drawArrays(gl.TRIANGLE_FAN, 0, Circle2D.vertexPositions.length);
    gl.disableVertexAttribArray(Circle2D.aPositionShader);

  }
}

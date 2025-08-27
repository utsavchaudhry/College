function getEllipseCoordinates(radius, segments = 360, offsetX = 0, offsetY = 0) {
  const coords = [];

  for (let i = 0; i < segments; i++) {
    const angle = (i / segments) * 2 * Math.PI;
    coords.push(vec2((radius * Math.cos(angle)) + offsetX, (radius * Math.sin(angle) * 0.6) + offsetY));
  }

  return coords;
}

function getCircleRedShading(segments = 360) {
  const colors = [];
  for (let i = 0; i < segments; i++) {
    const angle = (i / segments) * 2 * Math.PI;
    colors.push(vec3(red = (1 + Math.cos(angle)) / 2, 0.0, 0.0));
  }
  return colors;
}

class Ellipse2D {
  static vertexPositions = getEllipseCoordinates(0.15, 360, -0.4 ,0.35);
  static colors = getCircleRedShading(360);
  static shaderProgram = -1;
  static positionBuffer = -1;
  static colorBuffer = -1;
  static aPositionShader = -1;
  static aColorShader = -1;

  static initialize() {
    Ellipse2D.shaderProgram = initShaders(gl, "./vshader.glsl", "./fshader.glsl");

    Ellipse2D.positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Ellipse2D.positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(Ellipse2D.vertexPositions), gl.STATIC_DRAW);

	Ellipse2D.aPositionShader = gl.getAttribLocation(Ellipse2D.shaderProgram, "aPosition");

	Ellipse2D.colorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Ellipse2D.colorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(Ellipse2D.colors), gl.STATIC_DRAW);

	Ellipse2D.aColorShader = gl.getAttribLocation(Ellipse2D.shaderProgram, "aColor");
  }

  constructor() {
    if (Ellipse2D.shaderProgram === -1) Ellipse2D.initialize();
  }

  draw() {
    gl.useProgram(Ellipse2D.shaderProgram);

	gl.bindBuffer(gl.ARRAY_BUFFER, Ellipse2D.colorBuffer);
    gl.vertexAttribPointer(Ellipse2D.aColorShader, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Ellipse2D.aColorShader);

    gl.bindBuffer(gl.ARRAY_BUFFER, Ellipse2D.positionBuffer);
    gl.vertexAttribPointer(Ellipse2D.aPositionShader, 2, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Ellipse2D.aPositionShader);
    // Draw all 360 vertices of the circle
    gl.drawArrays(gl.TRIANGLE_FAN, 0, Ellipse2D.vertexPositions.length);
    gl.disableVertexAttribArray(Ellipse2D.aPositionShader);

  }
}

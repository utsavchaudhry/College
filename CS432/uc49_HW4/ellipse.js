function getEllipseCoordinates(radius, segments = 360, offsetX = 0, offsetY = 0) {
  const coords = [];

  for (let i = 0; i < segments; i++) {
    const angle = (i / segments) * 2 * Math.PI;
    coords.push(vec2((radius * Math.cos(angle)) + offsetX, (radius * Math.sin(angle) * 0.6) + offsetY));
  }

  return coords;
}

function getCircleGradient(
  baseColor,
  segments = 360,
  randomness = 0.1
) {
  const clamp = v => Math.min(1, Math.max(0, v));
  const randomOffset = () => (Math.random() * 2 - 1) * randomness;

  const startAngle = Math.random() * 2 * Math.PI;

  const colors = [];
  for (let i = 0; i < segments; i++) {
    const angle = startAngle + (i / segments) * 2 * Math.PI;

    const factor = (1 + Math.cos(angle)) / 2;

    const r = clamp(
      baseColor[0] + (1 - baseColor[0]) * factor + randomOffset()
    );
    const g = clamp(
      baseColor[1] + (1 - baseColor[1]) * factor + randomOffset()
    );
    const b = clamp(
      baseColor[2] + (1 - baseColor[2]) * factor + randomOffset()
    );

    colors.push(vec3(r, g, b));
  }
  return colors;
}

function rotateEllipse90(verts, cx, cy, clockwise = false) {
  for (let p of verts) {
    
    const dx = p[0] - cx;
    const dy = p[1] - cy;

    if (clockwise) {
      // (x',y') = ( y, -x )
      p[0] = cx +  dy;
      p[1] = cy + -dx;
    } else {
      // (x',y') = (-y,  x)
      p[0] = cx + -dy;
      p[1] = cy +  dx;
    }
  }
}


class Ellipse2D {
  static vertexPositions = getEllipseCoordinates(0.15, 360, -0.4 ,0.35);
  static colors = getCircleGradient(vec3(0.902, 0.224, 0.275), 360);
  static shaderProgram = -1;
  static positionBuffer = -1;
  static colorBuffer = -1;
  static aPositionShader = -1;
  static aColorShader = -1;

  static initialize() {

    Ellipse2D.shaderProgram = initShaders(gl, "./vshader.glsl", "./fshader.glsl");

    Ellipse2D.positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Ellipse2D.positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(Ellipse2D.vertexPositions), gl.DYNAMIC_DRAW);

	Ellipse2D.aPositionShader = gl.getAttribLocation(Ellipse2D.shaderProgram, "aPosition");

	Ellipse2D.colorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Ellipse2D.colorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(Ellipse2D.colors), gl.DYNAMIC_DRAW);

	Ellipse2D.aColorShader = gl.getAttribLocation(Ellipse2D.shaderProgram, "aColor");
  }

  constructor() {
    if (Ellipse2D.shaderProgram === -1) Ellipse2D.initialize();

    var myButton = document.getElementById("DirectionButton");
    myButton.addEventListener("click", () => {
      
      rotateEllipse90(Ellipse2D.vertexPositions, -0.4, 0.35);

      gl.bindBuffer(gl.ARRAY_BUFFER, Ellipse2D.positionBuffer);
      gl.bufferData(
        gl.ARRAY_BUFFER,
        flatten(Ellipse2D.vertexPositions),
        gl.DYNAMIC_DRAW
      );

    });


  }

  

  draw() {
    gl.useProgram(Ellipse2D.shaderProgram);

	  gl.bindBuffer(gl.ARRAY_BUFFER, Ellipse2D.colorBuffer);
    gl.vertexAttribPointer(Ellipse2D.aColorShader, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Ellipse2D.aColorShader);

    gl.bindBuffer(gl.ARRAY_BUFFER, Ellipse2D.positionBuffer);
    gl.vertexAttribPointer(Ellipse2D.aPositionShader, 2, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Ellipse2D.aPositionShader);
    
    gl.drawArrays(gl.TRIANGLE_FAN, 0, Ellipse2D.vertexPositions.length);
    gl.disableVertexAttribArray(Ellipse2D.aPositionShader);

  }
}

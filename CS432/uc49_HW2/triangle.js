
class Triangle2D {
  static vertexPositions = [
    	vec2(0,0.44),
    	vec2(-0.2,0.1),
    	vec2(0.2,0.1)];
  static colors  = [
    	vec3( 1.0, 0.0, 0.0 ),
    	vec3(  0.0, 1.0, 0.0 ),
    	vec3(  0.0, 0.0, 1.0 )];
  static shaderProgram = -1;
  static positionBuffer = -1;
  static colorBuffer = -1;
  static aPositionShader = -1;
  static aColorShader = -1;

  static initialize() {
    Triangle2D.shaderProgram = initShaders(gl, "./vshader.glsl", "./fshader.glsl");

    Triangle2D.positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(Triangle2D.vertexPositions), gl.STATIC_DRAW);

	Triangle2D.aPositionShader = gl.getAttribLocation(Triangle2D.shaderProgram, "aPosition");

	Triangle2D.colorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.colorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(Triangle2D.colors), gl.STATIC_DRAW);

	Triangle2D.aColorShader = gl.getAttribLocation(Triangle2D.shaderProgram, "aColor");
  }

  constructor() {
    if (Triangle2D.shaderProgram === -1) Triangle2D.initialize();
  }

  draw() {
    gl.useProgram(Triangle2D.shaderProgram);

	gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.colorBuffer);
    gl.vertexAttribPointer(Triangle2D.aColorShader, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Triangle2D.aColorShader);

    gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.positionBuffer);
    gl.vertexAttribPointer(Triangle2D.aPositionShader, 2, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Triangle2D.aPositionShader);
    // Draw all 360 vertices of the circle
    gl.drawArrays(gl.TRIANGLE_FAN, 0, Triangle2D.vertexPositions.length);
    gl.disableVertexAttribArray(Triangle2D.aPositionShader);

  }
}

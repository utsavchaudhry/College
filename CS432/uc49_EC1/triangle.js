
class Triangle2D {
  static vertexPositions = [
    	vec2(0,0.44),
    	vec2(-0.2,0.1),
    	vec2(0.2,0.1)];
  static shaderProgram = -1;
  static positionBuffer = -1;
  static colorBuffer = -1;
  static aPositionShader = -1;
  static aColorShader = -1;

  constructor(redColor, greenColor, blueColor, yellowColor, purpleColor, whiteColor) {
    
    this.redColor = redColor;
    this.greenColor = greenColor;
    this.blueColor = blueColor;
    this.yellowColor = yellowColor;
    this.purpleColor = purpleColor;
    this.whiteColor = whiteColor;

    if (Triangle2D.shaderProgram == -1) {
      Triangle2D.shaderProgram = initShaders(gl, "./vshader.glsl", "./fshader.glsl");
    }

    Triangle2D.positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(Triangle2D.vertexPositions), gl.DYNAMIC_DRAW);

	  Triangle2D.aPositionShader = gl.getAttribLocation(Triangle2D.shaderProgram, "aPosition");

	  Triangle2D.colorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.colorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, flatten(this.redColor), gl.DYNAMIC_DRAW);

	  Triangle2D.aColorShader = gl.getAttribLocation(Triangle2D.shaderProgram, "aColor");

    window.addEventListener("keydown", (event) => {
    
        const key = event.key.toLowerCase();

        switch (key) {
            case "r":
                gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.colorBuffer);
                gl.bufferData(
                  gl.ARRAY_BUFFER,
                  flatten(this.redColor),
                  gl.DYNAMIC_DRAW
                );
            break;
            case "g":  
                gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.colorBuffer);
                gl.bufferData(
                  gl.ARRAY_BUFFER,
                  flatten(this.greenColor),
                  gl.DYNAMIC_DRAW
                );
            break;
            case "b":  
                gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.colorBuffer);
                gl.bufferData(
                  gl.ARRAY_BUFFER,
                  flatten(this.blueColor),
                  gl.DYNAMIC_DRAW
                );
            break;
            case "y":  
                gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.colorBuffer);
                gl.bufferData(
                  gl.ARRAY_BUFFER,
                  flatten(this.yellowColor),
                  gl.DYNAMIC_DRAW
                );
            break;
            case "p":  
                gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.colorBuffer);
                gl.bufferData(
                  gl.ARRAY_BUFFER,
                  flatten(this.purpleColor),
                  gl.DYNAMIC_DRAW
                );
            break;
            case "w":  
                gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.colorBuffer);
                gl.bufferData(
                  gl.ARRAY_BUFFER,
                  flatten(this.whiteColor),
                  gl.DYNAMIC_DRAW
                );
            break;
            default:
            // ignore other keys
        }
    });

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

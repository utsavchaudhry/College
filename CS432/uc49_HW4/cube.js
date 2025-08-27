
function clampVec3(v, minVal, maxVal) {
  const x = Math.min(Math.max(v[0], minVal), maxVal);
  const y = Math.min(Math.max(v[1], minVal), maxVal);
  const z = Math.min(Math.max(v[2], minVal), maxVal);
  return vec3(x, y, z);
}

class Cube3D {
  
  static vertexPositions =  [
    vec3( -0.4, -0.4, -0.4 ),
    vec3( -0.4, 0.4, -0.4 ),
    vec3( 0.4, 0.4, -0.4 ),
    vec3( 0.4, -0.4, -0.4 ),
    vec3( -0.4, -0.4, 0.4 ),
    vec3( -0.4, 0.4, 0.4 ),
    vec3( 0.4, 0.4, 0.4 ),
    vec3( 0.4, -0.4, 0.4 )
    ];


  static colors =  [
    [ 1.0, 0.0, 0.0, 1.0 ], // red
    [ 1.0, 0.0, 1.0, 1.0 ], // magenta
    [ 1.0, 1.0, 1.0, 1.0 ], // white
    [ 1.0, 1.0, 0.0, 1.0 ], // yellow
    [ 0.0, 0.0, 0.0, 1.0 ], // black
    [ 0.0, 0.0, 1.0, 1.0 ], // blue
    [ 0.0, 1.0, 1.0, 1.0 ], // cyan
    [ 0.0, 1.0, 0.0, 1.0 ] // green
    ];

  static indices = [
    0, 1, 2,   0, 2, 3,    // front
    4, 5, 6,   4, 6, 7,    // back
    0, 4, 7,   0, 7, 3,    // bottom
    1, 5, 6,   1, 6, 2,    // top
    0, 1, 5,   0, 5, 4,    // left
    3, 2, 6,   3, 6, 7     // right
  ];

  static shaderProgram = -1;
  static positionBuffer = -1;
  static indexBuffer = -1;
  static colorBuffer = -1;
  static aPositionShader = -1;
  static aColorShader = -1;
  static translationLoc = -1;
  static rotationLoc = -1;
  static scaleLoc = -1;

  constructor() {
    
    this.reset();

    if (Cube3D.shaderProgram === -1) {
      
      Cube3D.shaderProgram = initShaders(gl, "./vshader.glsl", "./fshader.glsl");

      Cube3D.positionBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, Cube3D.positionBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, flatten(Cube3D.vertexPositions), gl.DYNAMIC_DRAW);

      Cube3D.aPositionShader = gl.getAttribLocation(Cube3D.shaderProgram, "aPosition");

      Cube3D.indexBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, Cube3D.indexBuffer);
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(Cube3D.indices), gl.STATIC_DRAW);

      Cube3D.colorBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, Cube3D.colorBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, flatten(Cube3D.colors), gl.DYNAMIC_DRAW);
      Cube3D.aColorShader = gl.getAttribLocation(Cube3D.shaderProgram, "aColor");

      Cube3D.translationLoc =  gl.getUniformLocation(Cube3D.shaderProgram, "translation");
      Cube3D.rotationLoc =  gl.getUniformLocation(Cube3D.shaderProgram, "rotation");
      Cube3D.scaleLoc =  gl.getUniformLocation(Cube3D.shaderProgram, "scale");
      
    } 

    var m = document.getElementById("mymenu");
		m.addEventListener("click", () => {

			this.mode = m.selectedIndex;

      this.modifyDelta(0.0);
			
		});

    window.addEventListener("keydown", (event) => {
    
        const key = event.key.toLowerCase();

        if (key == "r") {
          this.reset();
          return;
        }

        if (key == "m") {
          this.modifyDelta(0.001);
          return;
        }

        if (key == "n") {
          this.modifyDelta(-0.001);
          return;
        }

        switch (this.mode) {

          case 0:

            switch (key) {
              case "q":
                  this.currentPosition = add(this.currentPosition, vec3(0.0,0.0,-this.deltaT));
              break;
              case "w":  
                  this.currentPosition = add(this.currentPosition, vec3(0.0,this.deltaT,0.0));
              break;
              case "e":  
                  this.currentPosition = add(this.currentPosition, vec3(0.0,0.0,this.deltaT));
              break;
              case "a":  
                  this.currentPosition = add(this.currentPosition, vec3(-this.deltaT,0.0,0.0));
              break;
              case "s":  
                  this.currentPosition = add(this.currentPosition, vec3(0.0,-this.deltaT,0.0));
              break;
              case "d":  
                  this.currentPosition = add(this.currentPosition, vec3(this.deltaT,0.0,0.0));
              break;
              default:
              // ignore other keys
            }

          break;

          case 1:

            switch (key) {
              case "q":
                  this.currentRotation = add(this.currentRotation, vec3(0.0,0.0,-this.deltaR));
              break;
              case "d":  
                  this.currentRotation = add(this.currentRotation, vec3(0.0,this.deltaR,0.0));
              break;
              case "e":  
                  this.currentRotation = add(this.currentRotation, vec3(0.0,0.0,this.deltaR));
              break;
              case "w":  
                  this.currentRotation = add(this.currentRotation, vec3(-this.deltaR,0.0,0.0));
              break;
              case "a":  
                  this.currentRotation = add(this.currentRotation, vec3(0.0,-this.deltaR,0.0));
              break;
              case "s":  
                  this.currentRotation = add(this.currentRotation, vec3(this.deltaR,0.0,0.0));
              break;
              default:
              // ignore other keys
            }

          break;

          case 2:

            switch (key) {
              case "q":
                  this.currentScale = add(this.currentScale, vec3(0.0,0.0,-this.deltaS));
              break;
              case "w":  
                  this.currentScale = add(this.currentScale, vec3(0.0,this.deltaS,0.0));
              break;
              case "e":  
                  this.currentScale = add(this.currentScale, vec3(0.0,0.0,this.deltaS));
              break;
              case "a":  
                  this.currentScale = add(this.currentScale, vec3(-this.deltaS,0.0,0.0));
              break;
              case "s":  
                  this.currentScale = add(this.currentScale, vec3(0.0,-this.deltaS,0.0));
              break;
              case "d":  
                  this.currentScale = add(this.currentScale, vec3(this.deltaS,0.0,0.0));
              break;
              default:
              // ignore other keys
            }

            this.currentScale = clampVec3(this.currentScale, 0.2, 1.5);

          break;

        }
        
    });
  }

  modifyDelta(amnt) {

    const modeLabel = document.getElementById('modeLabel');
    const deltaDisplay = document.getElementById('modeValue');


    switch (this.mode) {
      case 0:
        this.deltaT = Math.round((Math.min(Math.max(this.deltaT + amnt, 0.002), 0.03)) * 1000) / 1000;
        modeLabel.textContent = 'Translation Delta: ';
        deltaDisplay.textContent = this.deltaT;
        break;
      case 1:
        this.deltaR = Math.round((Math.min(Math.max(this.deltaR + amnt, 0.002), 0.03)) * 1000) / 1000;
        modeLabel.textContent = 'Rotation Delta: ';
        deltaDisplay.textContent = this.deltaR;
        break;
      case 2:
        this.deltaS = Math.round((Math.min(Math.max(this.deltaS + amnt, 0.002), 0.03)) * 1000) / 1000;
        modeLabel.textContent = 'Scale Delta: ';
        deltaDisplay.textContent = this.deltaS;
        break;
    }
  }

  reset() {
    this.currentPosition = new vec3(0.0,0.0,0.0);
    this.currentRotation = new vec3(Math.PI/6, Math.PI/4, 0.0);
    this.currentScale = new vec3(1.0,1.0,1.0);
    this.mode = 0;
    this.deltaT = 0.01;
    this.deltaR = 0.01;
    this.deltaS = 0.01;
    this.modifyDelta(0.0);
  }

  draw() {
    gl.useProgram(Cube3D.shaderProgram);

    gl.uniform3fv(Cube3D.translationLoc, this.currentPosition);
    gl.uniform3fv(Cube3D.rotationLoc, this.currentRotation);
    gl.uniform3fv(Cube3D.scaleLoc, this.currentScale);

	  gl.bindBuffer(gl.ARRAY_BUFFER, Cube3D.colorBuffer);
    gl.vertexAttribPointer(Cube3D.aColorShader, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Cube3D.aColorShader);

    gl.bindBuffer(gl.ARRAY_BUFFER, Cube3D.positionBuffer);
    gl.vertexAttribPointer(Cube3D.aPositionShader, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(Cube3D.aPositionShader);

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, Cube3D.indexBuffer);
    gl.drawElements(gl.TRIANGLES, 36, gl.UNSIGNED_SHORT, 0);

    //gl.drawArrays(gl.TRIANGLE_FAN, 0, Cube3D.vertexPositions.length);
    gl.disableVertexAttribArray(Cube3D.aPositionShader);

    console.log(this.currentRotation);

  }
}

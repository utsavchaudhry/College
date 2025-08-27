class Square2D{

    static shaderProgram = -1;
    static aPositionShader = -1;
  	static aColorShader = -1;
    	
    constructor(vertices, colors){
        if (Square2D.shaderProgram == -1) {
			Square2D.shaderProgram = initShaders( gl, "./vshader.glsl", "./fshader.glsl");
		}

		this.positionBuffer = gl.createBuffer();
		gl.bindBuffer( gl.ARRAY_BUFFER, this.positionBuffer);
		gl.bufferData( gl.ARRAY_BUFFER, flatten(vertices), gl.STATIC_DRAW );
		Square2D.aPositionShader = gl.getAttribLocation( Square2D.shaderProgram, "aPosition" );

		this.colorBuffer = gl.createBuffer();
    	gl.bindBuffer(gl.ARRAY_BUFFER, this.colorBuffer);
		gl.bufferData(gl.ARRAY_BUFFER, flatten(colors), gl.STATIC_DRAW);
		Square2D.aColorShader = gl.getAttribLocation(Square2D.shaderProgram, "aColor");
    }

    draw() {

        gl.useProgram(Square2D.shaderProgram);

		gl.bindBuffer(gl.ARRAY_BUFFER, this.colorBuffer);
    	gl.vertexAttribPointer(Square2D.aColorShader, 3, gl.FLOAT, false, 0, 0);
    	gl.enableVertexAttribArray(Square2D.aColorShader);

        gl.bindBuffer( gl.ARRAY_BUFFER, this.positionBuffer);
       	gl.vertexAttribPointer(Square2D.aPositionShader, 2, gl.FLOAT, false, 0, 0 );
		gl.enableVertexAttribArray(Square2D.aPositionShader);    
	
    	gl.drawArrays( gl.TRIANGLE_FAN, 0, 4 );
    	gl.disableVertexAttribArray(Square2D.aPositionShader);    
    }
}


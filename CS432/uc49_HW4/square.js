class Square2D{

    static shaderProgram = -1;
    static aPositionShader = -1;
  	static aColorShader = -1;
    	
    constructor(vertices, colors1, colors2, colors3, colors4) {
        if (Square2D.shaderProgram == -1) {
			Square2D.shaderProgram = initShaders( gl, "./vshader.glsl", "./fshader.glsl");
		}

		this.positionBuffer = gl.createBuffer();
		gl.bindBuffer( gl.ARRAY_BUFFER, this.positionBuffer);
		gl.bufferData( gl.ARRAY_BUFFER, flatten(vertices), gl.DYNAMIC_DRAW );
		Square2D.aPositionShader = gl.getAttribLocation( Square2D.shaderProgram, "aPosition" );

		this.colorBuffer = gl.createBuffer();
    	gl.bindBuffer(gl.ARRAY_BUFFER, this.colorBuffer);
		gl.bufferData(gl.ARRAY_BUFFER, flatten(colors1), gl.DYNAMIC_DRAW);
		Square2D.aColorShader = gl.getAttribLocation(Square2D.shaderProgram, "aColor");

		this.colors1 = colors1;
		this.colors2 = colors2;
		this.colors3 = colors3;
		this.colors4 = colors4;

		var m = document.getElementById("mymenu");
		m.addEventListener("click", () => {

			gl.bindBuffer(gl.ARRAY_BUFFER, this.colorBuffer);

			switch (m.selectedIndex) {
				case 0:
					gl.bufferData(
						gl.ARRAY_BUFFER,
						flatten(this.colors1),
						gl.DYNAMIC_DRAW
					);
				break;
				case 1:
					gl.bufferData(
						gl.ARRAY_BUFFER,
						flatten(this.colors2),
						gl.DYNAMIC_DRAW
					);
				break;
				case 2:
					gl.bufferData(
						gl.ARRAY_BUFFER,
						flatten(this.colors3),
						gl.DYNAMIC_DRAW
					);
				break;
				case 3:
					gl.bufferData(
						gl.ARRAY_BUFFER,
						flatten(this.colors4),
						gl.DYNAMIC_DRAW
					);
				break;
			}
		});

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


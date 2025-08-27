class Square2D{
    static vertexPositions = [
    	vec2( -0.5, -0.5 ),
    	vec2(  -0.5,  0.5 ),
    	vec2(  0.5, 0.5 ),
    	vec2( 0.5, -0.5)];
			
    static shaderProgram = -1;
    static positionBuffer = -1;
    static aPositionShader = -1;
    
    static initialize() {
    	Square2D.shaderProgram = initShaders( gl, "./vshader.glsl", "./fshader.glsl");
    			
	// Load the data into the GPU
	Square2D.positionBuffer = gl.createBuffer();
	gl.bindBuffer( gl.ARRAY_BUFFER, Square2D.positionBuffer);
	gl.bufferData( gl.ARRAY_BUFFER, flatten(Square2D.vertexPositions), gl.STATIC_DRAW );
		
	// Associate our shader variables with our data buffer
	Square2D.aPositionShader = gl.getAttribLocation( Square2D.shaderProgram, "aPosition" );
    }
    	
    constructor(){
        if(Square2D.shaderProgram == -1)
            Square2D.initialize()
    }

    draw() {
        gl.useProgram(Square2D.shaderProgram);
        
        gl.bindBuffer( gl.ARRAY_BUFFER, Square2D.positionBuffer);
       	gl.vertexAttribPointer(Square2D.aPositionShader, 2, gl.FLOAT, false, 0, 0 );
       	
	gl.enableVertexAttribArray(Square2D.aPositionShader);    
	
    	gl.drawArrays( gl.TRIANGLE_FAN, 0, 4 );
    	gl.disableVertexAttribArray(Square2D.aPositionShader);    
    }
}


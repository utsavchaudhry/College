class Pentagon2D{
    static vertexPositions = [
    	vec2( -0.5, -0.5 ),
    	vec2(  -0.5,  0.5 ),
    	vec2(  0.5, 0.5 ),
		vec2(  0.75, 0.0 ),
    	vec2( 0.5, -0.5)];
			
    static shaderProgram = -1;
    static positionBuffer = -1;
    static aPositionShader = -1;
    
    static initialize() {
    	Pentagon2D.shaderProgram = initShaders( gl, "./vshader.glsl", "./fshader.glsl");
    			
	// Load the data into the GPU
	Pentagon2D.positionBuffer = gl.createBuffer();
	gl.bindBuffer( gl.ARRAY_BUFFER, Pentagon2D.positionBuffer);
	gl.bufferData( gl.ARRAY_BUFFER, flatten(Pentagon2D.vertexPositions), gl.STATIC_DRAW );
		
	// Associate our shader variables with our data buffer
	Pentagon2D.aPositionShader = gl.getAttribLocation( Pentagon2D.shaderProgram, "aPosition" );
    }
    	
    constructor(){
        if(Pentagon2D.shaderProgram == -1)
            Pentagon2D.initialize()
    }

    draw() {
        gl.useProgram(Pentagon2D.shaderProgram);
        
        gl.bindBuffer( gl.ARRAY_BUFFER, Pentagon2D.positionBuffer);
       	gl.vertexAttribPointer(Pentagon2D.aPositionShader, 2, gl.FLOAT, false, 0, 0 );
       	
	gl.enableVertexAttribArray(Pentagon2D.aPositionShader);    
	
    	gl.drawArrays( gl.TRIANGLE_FAN, 0, 5 );
    	gl.disableVertexAttribArray(Pentagon2D.aPositionShader);    
    }
}


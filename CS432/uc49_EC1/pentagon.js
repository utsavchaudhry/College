
function getRandomColorsVec3(count = 5) {
    const colors = [];
    while (colors.length < count) {
        colors.push(vec3(Math.random(), Math.random(), Math.random()));
    }
    return colors;
}


class Pentagon2D{
    		
    static shaderProgram = -1;
    static aPositionShader = -1;
	static aColorShader = -1;

    constructor(vertices) {
        if (Pentagon2D.shaderProgram == -1) {
            Pentagon2D.shaderProgram = initShaders(gl, "./vshader.glsl", "./fshader.glsl");
        }
        
        this.positionBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, this.positionBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, flatten(vertices), gl.STATIC_DRAW);
        
        this.numVertices = vertices.length;

        this.colorBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, this.colorBuffer);
        gl.bufferData(
            gl.ARRAY_BUFFER,
            flatten(getRandomColorsVec3(this.numVertices)),
            gl.DYNAMIC_DRAW
        );

        Pentagon2D.aPositionShader = gl.getAttribLocation(Pentagon2D.shaderProgram, "aPosition");
        Pentagon2D.aColorShader    = gl.getAttribLocation(Pentagon2D.shaderProgram, "aColor");
    }

    draw() {
        gl.useProgram(Pentagon2D.shaderProgram);

        gl.bindBuffer(gl.ARRAY_BUFFER, this.colorBuffer);
        gl.vertexAttribPointer(Pentagon2D.aColorShader, 3, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(Pentagon2D.aColorShader);

        gl.bindBuffer(gl.ARRAY_BUFFER, this.positionBuffer);
        gl.vertexAttribPointer(Pentagon2D.aPositionShader, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(Pentagon2D.aPositionShader);

        gl.drawArrays(gl.TRIANGLE_FAN, 0, this.numVertices);

        gl.disableVertexAttribArray(Pentagon2D.aPositionShader);
    }
}


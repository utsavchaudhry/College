
var canvas;
var gl;

var square1;
var square2;
var square3;
var square4;
var square5;
var square6;

var circle;
var ellipse;
var triangle;

var pentagons = []

function getGradient(
  baseColor,
  randomness = 0.2,
  asTriangle = false
) {
  const clamp = v => Math.min(1, Math.max(0, v));
  const randomOffset = () => (Math.random() * 2 - 1) * randomness;

  if (asTriangle) {
    const edges = [
      [0, 1],
      [1, 2],
      [2, 0],
    ];

    const fixedEdge = edges[Math.floor(Math.random() * edges.length)];

    const colors = [];
    for (let i = 0; i < 3; i++) {
      if (fixedEdge.includes(i)) {
        colors.push(vec3(...baseColor));
      } else {
        const grad = baseColor.map((c, idx) =>
          clamp(c + (1 - c) * 1 + randomOffset())
        );
        colors.push(vec3(...grad));
      }
    }
    return colors;
  } else {
    const corners = [
      [0, 0],
      [1, 0],
      [1, 1],
      [0, 1],
    ];
    const useMainDiagonal = Math.random() < 0.5;
    const fixedIndices = useMainDiagonal ? [0, 2] : [1, 3];

    const colors = [];
    for (let i = 0; i < 4; i++) {
      if (fixedIndices.includes(i)) {
        colors.push(vec3(...baseColor));
      } else {
        const [x, y] = corners[i];
        const factor = ((x + y) / 2) * randomness;
        const gradR = baseColor[0] + (1 - baseColor[0]) * factor;
        const gradG = baseColor[1] + (1 - baseColor[1]) * factor;
        const gradB = baseColor[2] + (1 - baseColor[2]) * factor;

        const r = clamp(gradR + randomOffset());
        const g = clamp(gradG + randomOffset());
        const b = clamp(gradB + randomOffset());

        colors.push(vec3(r, g, b));
      }
    }
    return colors;
  }
}

function getPolygonVertices(centerX, centerY, radius, numSides, rotation = 0) {
    const vertices = [];
    const angleIncrement = (2 * Math.PI) / numSides;
    for (let i = 0; i < numSides; i++) {
        const angle = rotation + i * angleIncrement;
        const x = centerX + radius * Math.cos(angle);
        const y = centerY + radius * Math.sin(angle);
        vertices.push(vec2(x, y));
    }
    return vertices;
}


function getPentagonVertices(centerX, centerY, radius, rotation = 0) {
  const vertices = [];
  const angleIncrement = (2 * Math.PI) / 5;

  for (let i = 0; i < 5; i++) {
    const angle = rotation + i * angleIncrement;
    const x = centerX + radius * Math.cos(angle);
    const y = centerY + radius * Math.sin(angle);
    vertices.push(vec2(x,y));
  }

  return vertices;
}

window.onload = function init(){
    canvas = document.getElementById( "gl-canvas" );
    gl = canvas.getContext('webgl2');
    if ( !gl ) { alert( "WebGL 2.0 isn't available" ); }

    gl.viewport( 0, 0, canvas.width, canvas.height );
    gl.clearColor( 0.0, 0.0, 0.0, 1.0 );

    ellipse = new Ellipse2D();
    circle = new Circle2D();

    redSquareColor    = getGradient(vec3(0.902, 0.224, 0.275), 0.1, true);
    greenSquareColor  = getGradient(vec3(0.180, 0.800, 0.443), 0.1, true);
    blueSquareColor   = getGradient(vec3(0.271, 0.482, 0.616), 0.1, true);
    purpleSquareColor = getGradient(vec3(0.514, 0.220, 0.925), 0.1, true);
    yellowSquareColor = getGradient(vec3(0.914, 0.769, 0.416), 0.1, true);
    blackSquareColor  = getGradient(vec3(0.000, 0.000, 0.000), 0.1, true);
    whiteSquareColor  = getGradient(vec3(1.000, 1.000, 1.000), 0.1, true);
    
    triangle = new Triangle2D(redSquareColor, greenSquareColor, blueSquareColor, yellowSquareColor, purpleSquareColor, whiteSquareColor);

    redSquareColor    = getGradient(vec3(0.902, 0.224, 0.275));
    greenSquareColor  = getGradient(vec3(0.180, 0.800, 0.443));
    blueSquareColor   = getGradient(vec3(0.271, 0.482, 0.616));
    purpleSquareColor = getGradient(vec3(0.514, 0.220, 0.925));
    yellowSquareColor = getGradient(vec3(0.914, 0.769, 0.416));
    blackSquareColor  = getGradient(vec3(0.000, 0.000, 0.000));
    whiteSquareColor  = getGradient(vec3(1.000, 1.000, 1.000));


    square1 = new Square2D([
    	vec2( -0.4, -0.8 ),
    	vec2(  -0.4, 0.0 ),
    	vec2(  0.4, 0.0 ),
    	vec2( 0.4, -0.8)],
        redSquareColor,
        greenSquareColor,
        blueSquareColor,
        purpleSquareColor
    );

    square2 = new Square2D([
    	vec2( -0.35, -0.75 ),
    	vec2(  -0.35, -0.05 ),
    	vec2(  0.35, -0.05 ),
    	vec2( 0.35, -0.75)],
        blackSquareColor,
        blackSquareColor,
        blackSquareColor,
        blackSquareColor
    );

    square3 = new Square2D([
    	vec2( -0.3, -0.7 ),
    	vec2(  -0.3, -0.1 ),
    	vec2(  0.3, -0.1 ),
    	vec2( 0.3, -0.7)],
        redSquareColor,
        greenSquareColor,
        blueSquareColor,
        purpleSquareColor
    );

    square4 = new Square2D([
    	vec2( -0.25, -0.65 ),
    	vec2(  -0.25, -0.15 ),
    	vec2(  0.25, -0.15 ),
    	vec2( 0.25, -0.65)],
        blackSquareColor,
        blackSquareColor,
        blackSquareColor,
        blackSquareColor
    );

    square5 = new Square2D([
    	vec2( -0.2, -0.6 ),
    	vec2(  -0.2, -0.2 ),
    	vec2(  0.2, -0.2 ),
    	vec2( 0.2, -0.6)],
        redSquareColor,
        greenSquareColor,
        blueSquareColor,
        purpleSquareColor
    );

    square6 = new Square2D([
    	vec2( -0.15, -0.55 ),
    	vec2(  -0.15, -0.25 ),
    	vec2(  0.15, -0.25 ),
    	vec2( 0.15, -0.55)],
        blackSquareColor,
        blackSquareColor,
        blackSquareColor,
        blackSquareColor
    );

    var myButton = document.getElementById("DirectionButton");
    myButton.addEventListener("click", () => {
      setTimeout(() => {
        render();
        }, 1);
    });
    

    var m = document.getElementById("mymenu");
    m.addEventListener("click", function() {
        
        setTimeout(() => {
                render();
            }, 1);

    });

    window.addEventListener("keydown", function(event) {
    
        const key = event.key.toLowerCase();

        const validKeys = ['r', 'g', 'b', 'y', 'p', 'w'];

        if (validKeys.includes(key)) {
            setTimeout(() => {
                render();
            }, 1);
        }

    });    

    let squareAngle = 0;
    let triangleAngle = 0;
    let circlePhase = Math.PI / 2;

    function animate() {
        
        squareAngle += 0.01;
        triangleAngle -= 0.01;
        circlePhase += 0.02;  

        const cosS = Math.cos(squareAngle);
        const sinS = Math.sin(squareAngle);
        const cosT = Math.cos(triangleAngle);
        const sinT = Math.sin(triangleAngle);

        const pivotSquare = vec2(0.0, -0.4);
        const pivotTriangle = vec2(0.0, 0.2133);

        // **Rotate each square around (0, -0.4)**
        // (We assume original vertex positions are stored in square.originalPositions)
        [square1, square2, square3, square4, square5, square6].forEach(square => {
            const newVerts = [];
            for (const v of square.originalPositions) {
                // Apply rotation formula: v' = pivot + R*(v - pivot)
                const dx = v[0] - pivotSquare[0];
                const dy = v[1] - pivotSquare[1];
                const rotatedX = pivotSquare[0] + dx * cosS - dy * sinS;
                const rotatedY = pivotSquare[1] + dx * sinS + dy * cosS;
                newVerts.push(vec2(rotatedX, rotatedY));
            }
            // Update the square's vertex buffer with new rotated coordinates
            gl.bindBuffer(gl.ARRAY_BUFFER, square.positionBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, flatten(newVerts), gl.DYNAMIC_DRAW);
        });

        const triVerts = Triangle2D.vertexPositions;
        const newTriVerts = [];
        for (const v of triVerts) {
            const dx = v[0] - pivotTriangle[0];
            const dy = v[1] - pivotTriangle[1];
            const rotatedX = pivotTriangle[0] + dx * cosT - dy * sinT;
            const rotatedY = pivotTriangle[1] + dx * sinT + dy * cosT;
            newTriVerts.push(vec2(rotatedX, rotatedY));
        }
        
        gl.bindBuffer(gl.ARRAY_BUFFER, Triangle2D.positionBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, flatten(newTriVerts), gl.DYNAMIC_DRAW);

        const baseRadius = 0.15;

        const scale = 0.5 * (1 + Math.sin(circlePhase));
        const currentRadius = baseRadius * scale;

        Circle2D.vertexPositions = getCircleCoordinates(currentRadius, 360, 0.4, 0.35);
        gl.bindBuffer(gl.ARRAY_BUFFER, Circle2D.positionBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, flatten(Circle2D.vertexPositions), gl.DYNAMIC_DRAW);

        render();

        requestAnimationFrame(animate);
    }

    animate();

    canvas.addEventListener('mousedown', function(event) {
        if (event.button !== 0) return;
        const rect = canvas.getBoundingClientRect();
        const x_ndc = (event.clientX - rect.left) / canvas.width * 2 - 1;
        const y_ndc = ((canvas.height - (event.clientY - rect.top)) / canvas.height) * 2 - 1;
        
        const sides = Math.floor(Math.random() * 16) + 4;
        
        const verts = getPolygonVertices(x_ndc, y_ndc, 0.05, sides, Math.random() * 2 * Math.PI);

        pentagons.push(new Pentagon2D(verts));
        render();  
    });




    render();
};

function render(){

    gl.clear( gl.COLOR_BUFFER_BIT );
    
    ellipse.draw();
    triangle.draw();
    circle.draw();
    
    square1.draw();
    square2.draw();
    square3.draw();
    square4.draw();
    square5.draw();
    square6.draw();

    for (const pent of pentagons) {
        pent.draw();
    }
}



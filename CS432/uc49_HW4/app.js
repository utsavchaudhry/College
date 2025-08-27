
var canvas;
var gl;

var cube;

window.onload = function init(){
    canvas = document.getElementById( "gl-canvas" );
    gl = canvas.getContext('webgl2');
    if ( !gl ) { alert( "WebGL 2.0 isn't available" ); }

    gl.viewport( 0, 0, canvas.width, canvas.height );
    gl.clearColor( 0.0, 0.0, 0.0, 1.0 );
    gl.enable(gl.DEPTH_TEST);
    gl.depthFunc(gl.LEQUAL);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    cube = new Cube3D();

    window.addEventListener("keydown", function(event) {
    
        const key = event.key.toLowerCase();

        const validKeys = ['q', 'w', 'e', 'a', 's', 'd', 'r'];

        if (validKeys.includes(key)) {
            setTimeout(() => {
                render();
            }, 1);
        }

    });

    render();
};

function render(){

    gl.clear( gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    
    cube.draw();
}



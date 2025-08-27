var canvas;
var gl;

var sq;

window.onload = function init(){
    canvas = document.getElementById( "gl-canvas" );
    gl = canvas.getContext('webgl2');
    if ( !gl ) { alert( "WebGL 2.0 isn't available" ); }

    gl.viewport( -canvas.width/5, canvas.height/5, canvas.width, canvas.height );
    gl.clearColor( 0.0, 0.0, 0.0, 1.0 );

    sq = new Square2D();
    
    render();
};

function render(){
    gl.clear( gl.COLOR_BUFFER_BIT );
    sq.draw();
}



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

window.onload = function init(){
    canvas = document.getElementById( "gl-canvas" );
    gl = canvas.getContext('webgl2');
    if ( !gl ) { alert( "WebGL 2.0 isn't available" ); }

    gl.viewport( -canvas.width/5, canvas.height/5, canvas.width, canvas.height );
    gl.clearColor( 0.0, 0.0, 0.0, 1.0 );

    ellipse = new Ellipse2D();
    triangle = new Triangle2D();
    circle = new Circle2D();
    
    square1 = new Square2D([
    	vec2( -0.4, -0.8 ),
    	vec2(  -0.4, 0.0 ),
    	vec2(  0.4, 0.0 ),
    	vec2( 0.4, -0.8)],
        [
    	vec3(  0.0, 1.0, 0.0 ),
    	vec3(  0.0, 0.0, 1.0 ),
    	vec3(  0.0, 1.0, 0.0 ),
        vec3(  0.0, 0.0, 1.0 )]
    );

    square2 = new Square2D([
    	vec2( -0.35, -0.75 ),
    	vec2(  -0.35, -0.05 ),
    	vec2(  0.35, -0.05 ),
    	vec2( 0.35, -0.75)],
        [
    	vec3(  0.0, 0.0, 1.0 ),
    	vec3(  0.0, 1.0, 0.0 ),
    	vec3(  0.0, 0.0, 1.0 ),
        vec3(  0.0, 1.0, 0.0 )]
    );

    square3 = new Square2D([
    	vec2( -0.3, -0.7 ),
    	vec2(  -0.3, -0.1 ),
    	vec2(  0.3, -0.1 ),
    	vec2( 0.3, -0.7)],
        [
    	vec3(  0.0, 1.0, 0.0 ),
    	vec3(  0.0, 0.0, 1.0 ),
    	vec3(  0.0, 1.0, 0.0 ),
        vec3(  0.0, 0.0, 1.0 )]
    );

    square4 = new Square2D([
    	vec2( -0.25, -0.65 ),
    	vec2(  -0.25, -0.15 ),
    	vec2(  0.25, -0.15 ),
    	vec2( 0.25, -0.65)],
        [
    	vec3(  0.0, 0.0, 1.0 ),
    	vec3(  0.0, 1.0, 0.0 ),
    	vec3(  0.0, 0.0, 1.0 ),
        vec3(  0.0, 1.0, 0.0 )]
    );

    square5 = new Square2D([
    	vec2( -0.2, -0.6 ),
    	vec2(  -0.2, -0.2 ),
    	vec2(  0.2, -0.2 ),
    	vec2( 0.2, -0.6)],
        [
    	vec3(  0.0, 1.0, 0.0 ),
    	vec3(  0.0, 0.0, 1.0 ),
    	vec3(  0.0, 1.0, 0.0 ),
        vec3(  0.0, 0.0, 1.0 )]
    );

    square6 = new Square2D([
    	vec2( -0.15, -0.55 ),
    	vec2(  -0.15, -0.25 ),
    	vec2(  0.15, -0.25 ),
    	vec2( 0.15, -0.55)],
        [
    	vec3(  0.0, 0.0, 1.0 ),
    	vec3(  0.0, 1.0, 0.0 ),
    	vec3(  0.0, 0.0, 1.0 ),
        vec3(  0.0, 1.0, 0.0 )]
    );
    
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
}



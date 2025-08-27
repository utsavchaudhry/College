
    // Get a file as a string using  AJAX
    // initShaders2.js

function loadFileAJAX(name) {
  var xhr = new XMLHttpRequest(),
      okStatus = location.protocol === "file:" ? 0 : 200;
  xhr.open('GET', name, false);
  xhr.send(null);
  return (xhr.status === okStatus) ? xhr.responseText : null;
}

function initShaders(gl, vsPath, fsPath) {
  // Helper: compile a shader, log any errors
  function compileShader(type, source, path) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
      console.error(`Error compiling shader (${path}):\n`, gl.getShaderInfoLog(shader));
      gl.deleteShader(shader);
      return null;
    }
    return shader;
  }

  // Load shader sources
  const vsSource = loadFileAJAX(vsPath);
  const fsSource = loadFileAJAX(fsPath);
  if (!vsSource || !fsSource) {
    console.error(`Failed to load shader sources from ${vsPath} or ${fsPath}`);
    return null;
  }

  // Compile vertex & fragment shaders
  const vertexShader   = compileShader(gl.VERTEX_SHADER,   vsSource, vsPath);
  const fragmentShader = compileShader(gl.FRAGMENT_SHADER, fsSource, fsPath);
  if (!vertexShader || !fragmentShader) {
    return null;
  }

  // Link into a program
  const program = gl.createProgram();
  gl.attachShader(program, vertexShader);
  gl.attachShader(program, fragmentShader);
  gl.linkProgram(program);

  // Check the link status
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    console.error("Error linking program:\n", gl.getProgramInfoLog(program));
    gl.deleteProgram(program);
    return null;
  }

  return program;
}


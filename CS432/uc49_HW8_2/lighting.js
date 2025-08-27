function Lighting() {
  // Light A: cylindrical around origin in WORLD space, then transformed to CAMERA space
  this.lightA = {
    angle: 0.0,             // radians
    radius: 6.0,
    height: 3.0
  };
  // Light B: fixed near the eye (in CAMERA space)
  this.lightB = {
    height: 3.0
  };
}

// Returns Light A position in CAMERA space given the camera (uses its view matrix)
Lighting.prototype.getLightAInCamera = function(camera) {
  const a = this.lightA.angle, r = this.lightA.radius, h = this.lightA.height;
  const pWorld = vec4(r * Math.cos(a), h, r * Math.sin(a), 1.0);
  const V = camera.getViewMatrix();
  const pCam4 = mult(V, pWorld);
  return vec3(pCam4[0], pCam4[1], pCam4[2]);
};

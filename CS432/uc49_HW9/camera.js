class Camera {
  constructor() {
    this.radius = 5.0;
    this.thetaDegrees = 0.0;
    this.phiDegrees = 45.0;
    this.at = vec3(0.0, 0.0, 0.0);
    this.up = vec3(0.0, 1.0, 0.0);
  }

  incrementTheta(deltaDeg) {
    this.thetaDegrees = (this.thetaDegrees + deltaDeg) % 360;
    if (this.thetaDegrees < 0) {
      this.thetaDegrees += 360;
    }
  }

  incrementPhi(deltaDeg) {
    this.phiDegrees += deltaDeg;
    if (this.phiDegrees < 5)   this.phiDegrees = 5;
    if (this.phiDegrees > 175) this.phiDegrees = 175;
  }
  
  zoom(factor) {
    this.radius *= factor;
    if (this.radius < 0.5)   this.radius = 0.5;
    if (this.radius > 50.0)  this.radius = 50.0;
  }
  
  setDistance(dist) {
    this.radius = dist;
    if (this.radius < 0.5)   this.radius = 0.5;
    if (this.radius > 50.0)  this.radius = 50.0;
  }

  reset() {
    this.radius = 5.0;
    this.thetaDegrees = 0.0;
    this.phiDegrees = 45.0;
  }

  getViewMatrix() {
    const phi   = this.phiDegrees * Math.PI / 180.0;
    const theta = this.thetaDegrees * Math.PI / 180.0;
    const eyeX = this.radius * Math.sin(phi) * Math.sin(theta);
    const eyeY = this.radius * Math.cos(phi);
    const eyeZ = this.radius * Math.sin(phi) * Math.cos(theta);
    const eye = vec3(eyeX, eyeY, eyeZ);
    return lookAt(eye, this.at, this.up);
  }
}

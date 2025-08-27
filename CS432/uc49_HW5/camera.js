"use strict";

class Camera {
  constructor(angle = 0, height = 0, radius = 2, perspective = true) {
    this.defaultAngle = angle;
    this.defaultHeight = height;
    this.defaultRadius = radius;
    this.defaultPerspective = perspective;
    this.angle = angle;       
    this.height = height;     
    this.radius = radius;     
    this.isPerspective = perspective;
  }

  incrementAngle(deltaDeg) {
    this.angle += deltaDeg;
    if (this.angle >= 360) this.angle -= 360;
    if (this.angle < 0) this.angle += 360;
  }

  incrementHeight(deltaH) {
    this.height += deltaH;
  }

  incrementRadius(deltaR) {
    this.radius += deltaR;
    if (this.radius < 0.1) this.radius = 0.1;
  }

  toggleProjection() {
    this.isPerspective = !this.isPerspective;
  }

  reset() {
    this.angle = this.defaultAngle;
    this.height = this.defaultHeight;
    this.radius = this.defaultRadius;
    this.isPerspective = this.defaultPerspective;
  }

  getViewMatrix() {
    const rad = radians(this.angle);
    const eye = vec3(
      this.radius * Math.sin(rad),
      this.height,
      this.radius * Math.cos(rad)
    );
    const at = vec3(0.0, 0.0, 0.0);
    const up = vec3(0.0, 1.0, 0.0);
    return lookAt(eye, at, up);
  }

  getProjectionMatrix(aspect) {
    if (this.isPerspective) {
      const fovy = 45.0;                
      const near = 0.1;
      const far = 100.0;
      return perspective(fovy, aspect, near, far);
    } else {
      const near = 0.1;
      const far = 100.0;
      if (aspect >= 1.0) {
        return ortho(-this.radius * aspect, this.radius * aspect,
                     -this.radius, this.radius, near, far);
      } else {
        return ortho(-this.radius, this.radius,
                     -this.radius / aspect, this.radius / aspect, near, far);
      }
    }
  }
}

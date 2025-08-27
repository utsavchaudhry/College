function Camera() {
  this.theta = 0.0;         // azimuth (radians)
  this.phi   = 0.3;         // elevation (radians)
  this.distance = 8.0;      // radius
  this.target = vec3(0.0, 0.0, 0.0);

  this.minPhi = -Math.PI/2 + 0.05;
  this.maxPhi =  Math.PI/2 - 0.05;
  this.minDist = 1.0;
}

Camera.prototype.setDistance = function(d) {
  this.distance = Math.max(this.minDist, d);
};

Camera.prototype.incrementTheta = function(deg) {
  this.theta += (deg * Math.PI / 180.0);
};

Camera.prototype.incrementPhi = function(deg) {
  const d = (deg * Math.PI / 180.0);
  this.phi = Math.min(this.maxPhi, Math.max(this.minPhi, this.phi + d));
};

Camera.prototype.zoom = function(scale) {
  this.distance = Math.max(this.minDist, this.distance * scale);
};

Camera.prototype.setTarget = function(v3) {
  this.target = vec3(v3[0], v3[1], v3[2]);
};

Camera.prototype.getEye = function() {
  const ct = Math.cos(this.theta), st = Math.sin(this.theta);
  const cp = Math.cos(this.phi),   sp = Math.sin(this.phi);
  return vec3(
    this.target[0] + this.distance * cp * st,
    this.target[1] + this.distance * sp,
    this.target[2] + this.distance * cp * ct
  );
};

// ModelView (view) matrix: look from eye to target with +Y up
Camera.prototype.getViewMatrix = function() {
  const eye = this.getEye();
  return lookAt(eye, this.target, vec3(0,1,0));
};

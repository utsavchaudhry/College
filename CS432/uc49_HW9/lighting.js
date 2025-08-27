class Lighting {
  constructor() {
    this.lightA = {
      angle: 0.0,
      radius: 2.0,
      height: 2.0
    };

    this.lightB = {
      height: 3.0
    };
  }

  getLightAInCamera(camera) {
    const x = this.lightA.radius * Math.cos(this.lightA.angle);
    const z = this.lightA.radius * Math.sin(this.lightA.angle);
    const y = this.lightA.height;
    const Lpos_world = vec3(x, y, z);
    const phi   = camera.phiDegrees * Math.PI / 180.0;
    const theta = camera.thetaDegrees * Math.PI / 180.0;
    const eye = vec3(
      camera.radius * Math.sin(phi) * Math.sin(theta),
      camera.radius * Math.cos(phi),
      camera.radius * Math.sin(phi) * Math.cos(theta)
    );
    const n = normalize(eye);
    const u = normalize(cross(camera.up, n));
    const v = cross(n, u);
    const Lpos_rel = subtract(Lpos_world, eye);
    const camX = u[0]*Lpos_rel[0] + u[1]*Lpos_rel[1] + u[2]*Lpos_rel[2];
    const camY = v[0]*Lpos_rel[0] + v[1]*Lpos_rel[1] + v[2]*Lpos_rel[2];
    const camZ = n[0]*Lpos_rel[0] + n[1]*Lpos_rel[1] + n[2]*Lpos_rel[2];
    return vec3(camX, camY, camZ);
  }
}

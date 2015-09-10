package scryetek.vecmath

/**
 * Created by Matt on 29/06/2014.
 */
class Quat(var x: Float, var y: Float, var z: Float, var w: Float) {
  def this() = this(0, 0, 0, 1)

  def set(x: Float, y: Float, z: Float, w: Float): Quat = {
    this.x = x; this.y = y; this.z = z; this.w = w;
    this
  }

  def set(q: Quat): Quat =
    set(q.x, q.y, q.z, q.w)


  def add(q: Quat): Quat = {
    x += q.x; y += q.y; z += q.z; w += q.w;
    this
  }

  // rotationTo
  // setAxis
  def setIdentity: Quat =
    set(0, 0, 0, 1)

  def setAngleAxis(angle: Float, axis: Vec3): Quat = {
    val halfAngle = angle * 0.5f
    val s = Math.sin(halfAngle).toFloat
    set(s*x, s*y, s*z, Math.cos(halfAngle).toFloat)
  }

  def mulInto(out: Quat, q: Quat): Quat =
    out.set(x*q.w + w*q.x + y*q.z - z*q.y,
        y*q.w + w*q.y + z*q.x - x*q.z,
        z*q.w + w*q.z + x*q.y - y*q.x,
        w*q.w - x*q.x - y*q.y - z*q.z)

  def mul(q: Quat): Quat =
    mulInto(this, q)

  def rotateXInto(out: Quat, angle: Float): Quat = {
    val halfAngle = angle*0.5f
    val bx = Math.sin(halfAngle).toFloat
    val bw = Math.cos(halfAngle).toFloat

    out.set(x*bw + w*bx, y*bw + z*bx, z*bw - y*bx, w*w  - x*bx)
  }

  def rotateX(angle: Float): Quat =
    rotateXInto(this, angle)

  def rotateYInto(out: Quat, angle: Float): Quat = {
    val halfAngle = angle*0.5f
    val by = Math.sin(halfAngle).toFloat
    val bw = Math.cos(halfAngle).toFloat

    set(x*bw - z*by, y*bw + w*by, z*bw + x*by, w*bw - y*by)
  }

  def rotateY(angle: Float): Quat =
    rotateYInto(this, angle)

  def rotateZInto(out: Quat, angle: Float): Quat = {
    val halfAngle = angle*0.5f
    val bz = Math.sin(halfAngle).toFloat
    val bw = Math.cos(halfAngle).toFloat

    out.set(x*bw + y*bz, y*bw - x*bz, z*bw + w*bz, w*bw - z*bz)
  }

  def rotateZ(angle: Float): Quat =
    rotateZInto(this, angle)

  def calculateWInto(out: Quat): Quat =
    out.set(x, y, z, -Math.sqrt(Math.abs(1-x*x - y*y - z*z)).toFloat)

  def calculateW: Quat =
    calculateWInto(this)

  def dot(q: Quat): Float =
    x*q.x + y*q.y + z*q.z + w*q.w

  // lerp
  def slerpInto(out: Quat, b: Quat, t: Float): Quat = {
    var bx = b.x; var by = b.y; var bz = b.z; var bw = b.w
    val cosom = dot(b)
    var scale0 = 0.0f; var scale1 = 0.0f
    if(cosom < 0) {
      bx = -bx
      by = -by
      bz = -bz
      bw = -bw
    }
    if(1.0f - cosom > 0.0001) {
      val omega = Math.acos(cosom).toFloat
      val sinom = Math.sin(omega).toFloat
      scale0 = Math.sin((1.0-t)*omega).toFloat/sinom
      scale1 = Math.sin(t*omega).toFloat/sinom
    } else {
      scale0 = 1.0f - t
      scale1 = t
    }

    out.set(scale0*x + scale1*bx,
            scale0*y + scale1*by,
            scale0*z + scale1*bz,
            scale0*w + scale1*bw)
  }

  def slerp(b: Quat, t: Float): Quat =
    slerpInto(this, b, t)

  def invertInto(out: Quat): Quat = {
    val dot = this.dot(this)
    val invDot = if(dot != 0) 1/dot else 0
    out.set(-x*invDot, -y*invDot, -z*invDot, w*invDot)
  }

  def invert: Quat =
    invertInto(this)

  def conjugateInto(q: Quat): Quat =
    q.set(-x, -y, -z, w)

  def conjugate: Quat =
    this.conjugateInto(this)

  def magnitude: Float =
    Math.sqrt(magnitude).toFloat

  def magSqr: Float =
    x*x + y*y + z*z + w*w

  def scale(s: Float): Quat = {
    x *= s; y *= s; z *= s; w*= s
    this
  }

  def normalize: Quat =
    scale(magnitude)

  // fromMat3
}

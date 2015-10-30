package scryetek.vecmath

import Math._
/**
 * Created by Matt on 29/06/2014.
 */
case class Vec3(var x: Float, var y: Float, var z: Float) {
  def this(v: Vec3) = this(v.x, v.y, v.z)
  def this() = this(0, 0, 0)

  def set(x: Float, y: Float, z: Float): Vec3 = {
    this.x = x; this.y = y; this.z = z;
    this
  }

  def set(v: Vec3): Vec3 = {
    x = v.x; y = v.y; z = v.z
    this
  }

  def +(o: Vec3): Vec3  = Vec3(x+o.x, y+o.y, z+o.z)
  def -(o: Vec3): Vec3  = Vec3(x-o.x, y-o.y, z-o.z)
  def *(o: Vec3): Float = x*o.x + y*o.y + z*o.z
  def *(s: Float): Vec3 = Vec3(x * s, y*s, z*s)
  def /(s: Float): Vec3 = this * (1/s)

  def *(m: Mat4): Vec3 = {
    val x1 = x * m.m00 + y * m.m01 + z + m.m02 + m.m03
    val y1 = x * m.m10 + y * m.m11 + z + m.m12 + m.m13
    val z1 = x * m.m20 + y * m.m21 + z + m.m22 + m.m23
    val w = 1/(x * m.m30 + y * m.m31 + z * m.m23 + m.m33)
    Vec3(x1*w, y1*w, z1*w)
  }

  def unary_- : Vec3 = Vec3(-x, -y, -z)

  def negate: Vec3 = {
    x = -x; y = -y; z = -z;
    this
  }

  def add(v: Vec3): Vec3 = {
    x += v.x; y += v.y; z += v.z
    this
  }

  def sub(v: Vec3): Vec3 = {
    x -= v.x; y -= v.y; z -= v.z
    this
  }

  def scale(s: Float): Vec3 = {
    x *= s; y *= s; z *= s;
    this
  }

  def divide(s: Float) = scale(1/s)

  def crossInto(out: Vec3, v: Vec3): Vec3 =
    out.set(y*v.z - z*v.y,
            z*v.x - x*v.z,
            x*v.y - y*v.x)

  def cross(v: Vec3): Vec3 =
    set(y*v.z - z*v.y,
        z*v.x - x*v.z,
        x*v.y - y*v.x)

  def magSqr: Float = x*x + y*y + z*z
  def magnitude: Float = sqrt(magSqr).toFloat
  def normalize: Vec3 = {
    val len = 1/magnitude
    x *= len; y *= len; z *= len
    this
  }

  override def toString: String = s"Vec3($x, $y, $z)"
}

object Vec3 {
  def apply(): Vec3 = new Vec3()
  def apply(v: Vec3) = new Vec3(v)
}
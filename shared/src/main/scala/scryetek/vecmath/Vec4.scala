package scryetek.vecmath

import java.lang.Math._

/**
 * Created by Matt on 29/06/2014.
 */
case class Vec4(var x: Float, var y: Float, var z: Float, var w: Float) {
  def this(v: Vec4) = this(v.x, v.y, v.z, v.w)
  def this() = this(0, 0, 0, 1)

  def set(x: Float, y: Float, z: Float, w: Float): Vec4 = {
    this.x = x; this.y = y; this.z = z; this.w = w
    this
  }

  def set(v: Vec4): Vec4 = {
    x = v.x; y = v.y; z = v.z; w = v.w
    this
  }

  def +(o: Vec4): Vec4  = Vec4(x+o.x, y+o.y, z+o.z, w+o.w)
  def -(o: Vec4): Vec4  = Vec4(x-o.x, y-o.y, z-o.z, w-o.w)
  def *(o: Vec4): Float = x*o.x + y*o.y + z*o.z + w*o.w
  def *(s: Float): Vec4 = Vec4(x*s, y*s, z*s, w*s)
  def /(s: Float): Vec4 = this * (1/s)

  def *(m: Mat4): Vec4 =
    Vec4(
      x * m.m00 + y * m.m01 + z * m.m02 + w * m.m03,
      x * m.m10 + y * m.m11 + z * m.m12 + w * m.m13,
      x * m.m20 + y * m.m21 + z * m.m22 + w * m.m23,
      x * m.m30 + y * m.m31 + z * m.m32 + w * m.m33)

  def unary_- : Vec4 = new Vec4(-x, -y, -z, -w)

  def negate: Vec4 = {
    x = -x; y = -y; z = -z; w = -w
    this
  }

  def add(v: Vec4): Vec4 = {
    x += v.x; y += v.y; z += v.z; w += v.w
    this
  }

  def sub(v: Vec4): Vec4 = {
    x -= v.x; y -= v.y; z -= v.z; w -= v.w
    this
  }

  def scale(s: Float): Vec4 = {
    x *= s; y *= s; z *= s; w *= s
    this
  }

  def divide(s: Float) = scale(1/s)

  def magSqr: Float = x*x + y*y + z*z + w*w
  def magnitude: Float = sqrt(magSqr).toFloat
  def normalize: Vec4 = {
    val len = 1/magnitude
    x *= len; y *= len; z *= len; w *= len
    this
  }

  override def toString: String = s"Vec4($x, $y, $z, $w)"
}

object Vec4 {
  def apply() = new Vec4()
  def apply(v: Vec4): Vec4 = new Vec4(v)
}
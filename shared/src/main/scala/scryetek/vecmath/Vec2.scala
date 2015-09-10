package scryetek.vecmath

/**
 * Created by Matt on 05/07/2014.
 */
case class Vec2(var x: Float, var y: Float) {
  def this() = this(0, 0)
  def this(v: Vec2) = this(v.x, v.y)

  def set(x: Float, y: Float): Vec2 = {
    this.x = x
    this.y = y
    this
  }

  def set(v: Vec2): Vec2 = {
    this.x = v.x
    this.y = v.y
    this
  }

  def +(o: Vec2): Vec2  = Vec2(x+o.x, y+o.y)
  def -(o: Vec2): Vec2  = Vec2(x-o.x, y-o.y)
  def *(o: Vec2): Float = x*o.x + y*o.y
  def *(s: Float): Vec2 = Vec2(x * s, y*s)
  def /(s: Float): Vec2 = this * (1/s)

  def unary_- : Vec2 = Vec2(-x, -y)

  def add(v: Vec2): Vec2 = {
    this.x += v.x
    this.y += v.y
    this
  }

  def sub(v: Vec2): Vec2 = {
    this.x -= v.x
    this.y -= v.y
    this
  }

  def scale(s: Float): Vec2 = {
    x *= s; y *= s
    this
  }

  def divide(s: Float): Vec2 = scale(1/s)

  def magSqr: Float = x*x + y*y
  def magnitude: Float = Math.sqrt(x*x + y*y).toFloat

  def normalize =
    scale(1/magnitude)

  def negate: Vec2 = {
    this.x = -x
    this.y = -y
    this
  }

  def dot(v: Vec2) = x*v.x + y*v.y

  def zNormal(v: Vec2) = x*v.y - y*v.x

  def max(v: Vec2) = Vec2(x max v.x, y max v.y)
  def min(v: Vec2) = Vec2(x min v.x, y min v.y)

  override def toString: String = s"Vec2($x, $y)"
}

object Vec2 {
  def apply() = new Vec2()
  def apply(v: Vec2) = new Vec2(v)
}
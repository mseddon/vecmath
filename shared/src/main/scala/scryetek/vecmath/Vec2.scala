package scryetek.vecmath

@inline
final class Vec2(var x: Float, var y: Float) {
  @inline
  def this() = this(0, 0)

  @inline
  def set(v: Vec2): Vec2 =
    set(v.x, v.y)

  @inline
  def set(x: Float = this.x, y: Float = this.y): Vec2 = {
    this.x = x
    this.y = y
    this
  }

  /** Adds two vectors. */
  @inline
  def +(v: Vec2): Vec2 =
    Vec2(x + v.x, y + v.y)

  /** Adds this vector to another vector into the target output vector. */
  @inline
  def add(v: Vec2, out: Vec2 = this): Vec2 =
    out.set(x + v.x, y + v.y)

  /** Adds this vector to another vector into the target output vector. */
  @inline
  def add(x: Float, y: Float): Vec2 =
    add(x, y, this)

  /** Adds this vector to another vector into the target output vector. */
  @inline
  def add(x: Float, y: Float, out: Vec2): Vec2 =
    out.set(this.x + x, this.y + y)

  /** Subtracts two vectors. */
  @inline
  def -(v: Vec2): Vec2 =
    Vec2(x - v.x, y - v.y)

  /** Subtracts a vector from this vector into the given output vector. */
  @inline
  def sub(v: Vec2, out: Vec2 = this): Vec2 =
    out.set(x - v.x, y - v.y)

  /** Subtracts a vector from this vector into the given output vector. */
  @inline
  def sub(x: Float, y: Float): Vec2 =
    sub(x, y, this)

  /** Subtracts a vector from this vector into the given output vector. */
  @inline
  def sub(x: Float, y: Float, out: Vec2): Vec2 =
    out.set(this.x - x, this.y - y)

  /** The dot product of two vectors. */
  @inline
  def *(v: Vec2): Float =
    x*v.x + y*v.y

  /** Returns the vector scaled by the given scalar. */
  @inline
  def *(s: Float): Vec2 =
    Vec2(x*s, y*s)

  /** Scales this vector by the given scalar, into the target output vector. */
  @inline
  def scale(s: Float, out: Vec2 = this): Vec2 =
    out.set(x*s, y*s)

  /** Returns the vector dividied by the given scalar. */
  @inline
  def /(s: Float): Vec2 = {
    val f = 1/s
    Vec2(x*f, y*f)
  }

  /** Divides this vector by the given scalar into the target output vector. */
  @inline
  def div(s: Float, out: Vec2 = this): Vec2 =
    scale(1/s, out)

  /** Negates this vector. */
  @inline
  def unary_- =
    Vec2(-x, -y)

  /** Negates this vector into the target output vector. */
  @inline
  def negate(out: Vec2 = this): Vec2 =
    out.set(-x, -y)
  
  /** Returns the squared magnitude (length<sup>2</sup>) of this vector. */
  @inline
  def magSqr = x*x + y*y

  /** Returns the magnitude (length) of this vector. */
  @inline
  def magnitude = math.sqrt(magSqr).toFloat

  /** Returns the normalized vector. */
  @inline
  def normalized = this / magnitude

  /** Normalizes this vector into the target output vector. */
  @inline
  def normalize(out: Vec2 = this): Vec2 =
    out.set(this).div(magnitude)

  @inline
  def zNormal(v: Vec2) = x*v.y - y*v.x

  @inline
  def max(v: Vec2): Vec2 =
    Vec2(v.x max x, v.y max y)

  @inline
  def min(v: Vec2): Vec2 =
    Vec2(v.x min x, v.y min y)

  @inline
  def copy(x: Float = x, y: Float = y): Vec2 =
    Vec2(x, y)


  /**
   * Return a vector reflecting this vector about the given normal.
   * @note the normal must be normalized.
   */
  def reflected(normal: Vec2): Vec2 =
    normal * 2*(this*normal) - this

  /**
   * Returns the angle between this vector and another, such that a rotation about this angle will align this vector onto the other.
   */
  def angleBetween(v: Vec2): Float = {
    def wrapPi(angle: Float): Float =
      if(angle < -math.Pi) angle + 2*math.Pi.toFloat
      else if(angle > math.Pi) angle - 2*math.Pi.toFloat
      else angle
    wrapPi(math.atan2(v.y, v.x).toFloat - math.atan2(y, x).toFloat)
  }

  /**
   * Destructively reflect this vector about the given normal.
   * @note the normal must be normalized.
   */
  def reflect(normal: Vec2, out: Vec2 = this): Vec2 = {
    val scale = 2*(this*normal)
    out.set(normal.x*scale-x, normal.y*scale-y)
  }
  /**
   * Returns the linear interpolation of this vector with another, with t ranging from 0..1
   */
  @inline
  def lerp(q: Vec2, t: Float): Vec2 =
    Vec2(x + t*(q.x-x),
         y + t*(q.y-y))

  /**
   * Destructively places the linear interpolation of this vector with another into out, with t ranging from 0..1
   */
  def lerp(q: Vec2, t: Float, out: Vec2): Vec2 =
    out.set(x + t*(q.x-x),
            y + t*(q.y-y))

  override def toString =
    s"Vec2(${x}f,${y}f)"

  override def equals(o: Any): Boolean = o match {
    case v: Vec2 => x == v.x && y == v.y
    case _ => false
  }

  override def hashCode: Int =
    x.hashCode() * 19 +  y.hashCode() * 23
}

object Vec2 {
  def apply() = new Vec2()
  def apply(x: Float, y: Float) = new Vec2(x, y)
}
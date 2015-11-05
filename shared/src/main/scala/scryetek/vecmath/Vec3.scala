package scryetek.vecmath

@inline
final class Vec3(var x: Float, var y: Float, var z: Float) {
  @inline
  def this() = this(0, 0, 0)

  @inline
  def set(x: Float = this.x, y: Float = this.y, z: Float = this.z) = {
    this.x = x
    this.y = y
    this.z = z
    this
  }

  @inline
  def set(v: Vec3): Vec3 =
    set(v.x, v.y, v.z)

  /** Adds two vectors. */
  @inline
  def +(v: Vec3): Vec3 =
    Vec3(x + v.x, y + v.y, z + v.z)

  /** Adds this vector to another vector into the target output vector. */
  @inline
  def add(v: Vec3, out: Vec3 = this): Vec3 =
    out.set(x + v.x, y + v.y, z + v.z)

  /** Adds this vector to another vector into the target output vector. */
  @inline
  def add(x: Float, y: Float, z: Float): Vec3 =
    add(x, y, z, this)

  /** Adds this vector to another vector into the target output vector. */
  @inline
  def add(x: Float, y: Float, z: Float, out: Vec3): Vec3 =
    out.set(this.x + x, this.y + y, this.z + z)

  /** Subtracts two vectors. */
  @inline
  def -(v: Vec3): Vec3 =
    Vec3(x - v.x, y - v.y, z - v.z)

  /** Subtracts a vector from this vector into the given output vector. */
  @inline
  def sub(v: Vec3, out: Vec3 = this): Vec3 =
    out.set(x - v.x, y - v.y, z - v.z)

  /** Subtracts a vector from this vector into the given output vector. */
  @inline
  def sub(x: Float, y: Float, z: Float, out: Vec3): Vec3 =
    out.set(this.x - x, this.y - y, this.z - z)

  /** Subtracts a vector from this vector into the given output vector. */
  @inline
  def sub(x: Float, y: Float, z: Float): Vec3 =
    sub(x, y, z, this)

  /** The dot product of two vectors. */
  @inline
  def *(v: Vec3): Float =
    x*v.x + y*v.y + z*v.z

  /** Returns the vector scaled by the given scalar. */
  @inline
  def *(s: Float): Vec3 =
    Vec3(x*s, y*s, z*s)

  /** Scales this vector by the given scalar, into the target output vector. */
  def scale(s: Float, out: Vec3 = this): Vec3 =
    out.set(x*s, y*s, z*s)

  /** Returns the vector dividied by the given scalar. */
  @inline
  def /(s: Float): Vec3 = {
    val f = 1/s
    Vec3(x*f, y*f, z*f)
  }

  @inline
  def div(s: Float, out: Vec3 = this): Vec3 =
    scale(1/s, out)

  @inline
  def unary_- =
    Vec3(-x, -y, -z)

  @inline
  def negate(out: Vec3 = this) =
    out.set(-x, -y, -z)

  /** Returns the squared magnitude (length<sup>2</sup>) of this vector. */
  @inline
  def magSqr = x*x + y*y + z*z

  /** Returns the magnitude (length) of this vector. */
  @inline
  def magnitude = math.sqrt(magSqr).toFloat

  /** Returns the normalized vector. */
  @inline
  def normalized = this / magnitude

  @inline
  def normalize(out: Vec3 = this) =
    out.set(this).div(magnitude)

  /** Returns the cross product of two vectors. */
  @inline
  def ⨯(v: Vec3): Vec3 =
    Vec3(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x)

  /** Returns the cross product of two vectors. */
  @inline
  def crossed(v: Vec3): Vec3 = this ⨯ v

  /** Cross products this vector and another into the target output vector. */
  @inline
  def cross(v: Vec3, out: Vec3 = this) =
    out.set(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x)

  @inline
  def max(v: Vec3): Vec3 =
    Vec3(v.x max x, v.y max y, v.z max z)

  @inline
  def min(v: Vec3): Vec3 =
    Vec3(v.x min x, v.y min y, v.z min z)

  /**
   * Return the quaternion that will align this vector with another.
   */
  def angleBetween(b: Vec3): Quat = {
    val mag = magnitude*b.magnitude
    val qx = y*b.z - z*b.y
    val qy = z*b.x - x*b.z
    val qz = x*b.y - y*b.x
    val qw = mag+(this*b)

    Quat(qx, qy, qz, qw).normalized
  }

  /**
   * Return a vector reflecting this vector about the given normal.
   * @note the normal must be normalized.
   */
  def reflected(normal: Vec3): Vec3 =
    normal * 2*(this*normal) - this

  /**
   * Destructively reflect this vector about the given normal.
   */
  def reflect(normal: Vec3, out: Vec3 = this): Vec3 = {
    val scale = 2*(this*normal)
    out.set(normal.x*scale-x, normal.y*scale-y, normal.z*scale-z)
  }

  /**
   * Returns the linear interpolation of this vector with another, with t ranging from 0..1
   */
  @inline
  def lerp(q: Vec3, t: Float): Vec3 =
    Vec3(x + t*(q.x-x),
         y + t*(q.y-y),
         z + t*(q.z-z))

  /**
   * Destructively places the linear interpolation of this vector with another into out, with t ranging from 0..1
   */
  def lerp(q: Vec3, t: Float, out: Vec3): Vec3 =
    out.set(x + t*(q.x-x),
            y + t*(q.y-y),
            z + t*(q.z-z))

  @inline
  def copy(x: Float = x, y: Float = y, z: Float = z): Vec3 =
    Vec3(x, y, z)

  override def toString =
    s"Vec3(${x}f,${y}f,${z}f)"


  override def equals(o: Any): Boolean = o match {
    case v: Vec3 => x == v.x && y == v.y && z == v.z
    case _ => false
  }

  override def hashCode: Int =
    x.hashCode()*19 +  y.hashCode()*23 + z.hashCode()*29
}

object Vec3 {
  def apply(x: Float, y: Float, z: Float) = new Vec3(x, y, z)
  def apply() = new Vec3()
}
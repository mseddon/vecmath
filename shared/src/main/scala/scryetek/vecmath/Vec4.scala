package scryetek.vecmath

@inline
final class Vec4(var x: Float, var y: Float, var z: Float, var w: Float) {
  def this() = this(0, 0, 0, 0)

  @inline
  def set(x: Float = this.x, y: Float = this.y, z: Float = this.z, w: Float = this.w) = {
    this.x = x
    this.y = y
    this.z = z
    this.w = w
    this
  }

  @inline
  def set(v: Vec4): Vec4 =
    set(v.x, v.y, v.z, v.w)

  /** Adds two vectors. */
  @inline
  def +(v: Vec4): Vec4 =
    Vec4(x + v.x, y + v.y, z + v.z, w + v.w)

  @inline
  def add(v: Vec4, out: Vec4 = this): Vec4 =
    out.set(x + v.x, y + v.y, z + v.z, w + v.w)

  @inline
  def add(x: Float, y: Float, z: Float, w: Float): Vec4 =
    add(x, y, z, w, this)

  @inline
  def add(x: Float, y: Float, z: Float, w: Float, out: Vec4): Vec4 =
    out.set(this.x + x, this.y + y, this.z + z, this.w + w)

  /** Subtracts two vectors. */
  @inline
  def -(v: Vec4): Vec4 =
    Vec4(x - v.x, y - v.y, z - v.z, w - v.w)

  @inline
  def sub(v: Vec4, out: Vec4 = this): Vec4 =
    out.set(x - v.x, y - v.y, z - v.z, w - v.w)

  @inline
  def sub(x: Float, y: Float, z: Float, w: Float, out: Vec4): Vec4 =
    out.set(this.x - x, this.y - y, this.z - z, this.w - w)

  @inline
  def sub(x: Float, y: Float, z: Float, w: Float): Vec4 =
    sub(x, y, z, w, this)

  /** The dot product of two vectors. */
  @inline
  def *(v: Vec4): Float =
    x*v.x + y*v.y + z*v.z + z*v.z

  /** Returns the vector scaled by the given scalar. */
  @inline
  def *(s: Float): Vec4 =
    Vec4(x*s, y*s, z*s, w*s)

  @inline
  def scale(s: Float, out: Vec4 = this): Vec4 =
    out.set(x*s, y*s, z*s, w*s)

  /** Returns the vector dividied by the given scalar. */
  @inline
  def /(s: Float): Vec4 = {
    val f = 1/s
    Vec4(x*f, y*f, z*f, w*f)
  }

  @inline
  def div(s: Float, out: Vec4 = this): Vec4 =
    scale(1/s, out)

  @inline
  def unary_- =
    Vec4(-x, -y, -z, -w)

  @inline
  def negate(out: Vec4 = this) =
    out.set(-x, -y, -z, -w)

  /** Returns the squared magnitude (length<sup>2</sup>) of this vector. */
  @inline
  def magSqr = x*x + y*y + z*z + w*w

  @inline
  /** Returns the magnitude (length) of this vector. */
  def magnitude = math.sqrt(magSqr).toFloat

  /** Returns the normalized vector. */
  @inline
  def normalized = this / magnitude

  @inline
  def normalize(out: Vec4 = this) =
    out.set(this).div(magnitude)
  
  @inline
  def max(v: Vec4): Vec4 =
    Vec4(v.x max x, v.y max y, v.z max z, v.w max w)

  @inline
  def min(v: Vec4): Vec4 =
    Vec4(v.x min x, v.y min y, v.z min z, v.w max w)

  @inline
  def copy(x: Float = x, y: Float = y, z: Float = z, w: Float = w) =
    new Vec4(x,y,z,w)

  /**
   * Returns the linear interpolation of this vector with another, with t ranging from 0..1
   */
  @inline
  def lerp(q: Vec4, t: Float): Vec4 =
    Vec4(x + t*(q.x-x),
         y + t*(q.y-y),
         z + t*(q.z-z),
         w + t*(q.w-w))

  /**
   * Destructively places the linear interpolation of this vector with another into out, with t ranging from 0..1
   */
  def lerp(q: Vec4, t: Float, out: Vec4): Vec4 =
    out.set(x + t*(q.x-x),
            y + t*(q.y-y),
            z + t*(q.z-z),
            w + t*(q.w-w))

  override def toString =
    s"Vec4(${x}f,${y}f,${z}f,${w}f})"

  override def equals(o: Any): Boolean = o match {
    case v: Vec4 => x == v.x && y == v.y && z == v.z && w == v.w
    case _ => false
  }

  override def hashCode: Int =
    x.hashCode()*19 +  y.hashCode()*23 + z.hashCode()*29 + w.hashCode()*31
}

object Vec4 {
  def apply(x: Float, y: Float, z: Float, w: Float) = new Vec4(x,y,z,w)
  def apply() = new Vec4()
}
package scryetek.vecmath

/**
 * Represents a 3x3 matrix with the bottom row set to 0 0 1.
 */
final class Mat2d(
  var m00: Float, var m01: Float, var m02: Float,
  var m10: Float, var m11: Float, var m12: Float) {
  @inline
  def this() = this(1, 0, 0,
                    0, 1, 0)

  @inline
  def set(m00: Float = m00, m01: Float = m01, m02: Float = m02,
          m10: Float = m10, m11: Float = m11, m12: Float = m12): Mat2d = {
    this.m00 = m00; this.m01 = m01; this.m02 = m02
    this.m10 = m10; this.m11 = m11; this.m12 = m12
    this
  }

  def set(m: Mat2d): Mat2d =
    Mat2d(m.m00, m.m01, m.m02,
          m.m10, m.m11, m.m12)

  @inline
  def inverted = {
    var det = determinant
    assert(det != 0, "Matrix is not invertable")
    det = 1/det
    Mat2d( m11*det, -m01*det, (m01*m12 - m02*m11)*det,
          -m10*det, m00*det,  (m02*m10 - m00*m12)*det)
  }

  @inline
  def invert(out: Mat2d = this) = {
    var det = determinant
    assert(det != 0, "Matrix is not invertable")
    det = 1/det
    out.set( m11*det, -m01*det, (m01*m12 - m02*m11)*det,
            -m10*det, m00*det,  (m02*m10 - m00*m12)*det)
  }
  
  @inline
  def determinant =
    m00*m11 - m01*m10

  /** Adds two matrices */
  @inline
  def +(m: Mat2d): Mat2d =
    Mat2d(m00 + m.m00, m01 + m.m01, m02 + m.m02,
          m10 + m.m10, m11 + m.m11, m12 + m.m12)

  /** Destructively add another matrix to this matrix into the output matrix. */
  @inline
  def add(m: Mat2d, out: Mat2d = this): Mat2d =
    out.set(m00 + m.m00, m01 + m.m01, m02 + m.m02,
            m10 + m.m10, m11 + m.m11, m12 + m.m12)

  /** Subtracts two matrices */
  @inline
  def -(m: Mat2d): Mat2d =
    Mat2d(m00 - m.m00, m01 - m.m01, m02 - m.m02,
          m10 - m.m10, m11 - m.m11, m12 - m.m12)

  /** Destructively add another matrix to this matrix into the output matrix. */
  @inline
  def sub(m: Mat2d, out: Mat2d = this): Mat2d =
    out.set(m00 - m.m00, m01 - m.m01, m02 - m.m02,
            m10 - m.m10, m11 - m.m11, m12 - m.m12)

  @inline
  def *(v: Vec3): Vec3 =
    Vec3(m00*v.x + m01*v.y + m02*v.z,
         m10*v.x + m11*v.y + m12*v.z,
         v.z)

  @inline
  def *(s: Float): Mat2d =
    Mat2d(m00*s, m01*s, m02*s,
      m10*s, m11*s, m12*s)

  @inline
  def scale(s: Float, out: Mat2d = this): Mat2d =
    out.set(m00*s, m01*s, m02*s,
            m10*s, m11*s, m12*s)

  @inline
  def mul(v: Vec3, out: Vec3): Vec3 =
    out.set(m00*v.x + m01*v.y + m02*v.z,
            m10*v.x + m11*v.y + m12*v.z,
            v.z)

  @inline
  def mul(v: Vec3): Vec3 =
    mul(v, v)

  @inline
  def *(v: Vec2): Vec2 =
    Vec2(v.x * m00 + v.y * m01 + m02,
      v.x * m10 + v.y * m11 + m12)

  @inline
  def mul(v: Vec2, out: Vec2): Vec2 =
    out.set(v.x * m00 + v.y * m01 + m02,
      v.x * m10 + v.y * m11 + m12)

  @inline
  def mul(v: Vec2): Vec2 =
    mul(v, v)

  @inline
  def *(m: Mat2d): Mat2d =
    Mat2d(m.m00*m00 + m.m10*m01, m.m01*m00 + m.m11*m01, m.m02*m00 + m.m12*m01 + m02,
          m.m00*m10 + m.m10*m11, m.m01*m10 + m.m11*m11, m.m02*m10 + m.m12*m11 + m12)

  @inline
  def postMultiply(m: Mat2d, out: Mat2d = this): Mat2d =
    out.set(m.m00*m00 + m.m10*m01, m.m01*m00 + m.m11*m01, m.m02*m00 + m.m12*m01 + m02,
      m.m00*m10 + m.m10*m11, m.m01*m10 + m.m11*m11, m.m02*m10 + m.m12*m11 + m12)

  @inline
  def preMultiply(m: Mat2d, out: Mat2d = this): Mat2d =
    out.set(m00*m.m00 + m10*m.m01, m01*m.m00 + m11*m.m01, m02*m.m00 + m12*m.m01 + m.m02,
            m00*m.m10 + m10*m.m11, m01*m.m10 + m11*m.m11, m02*m.m10 + m12*m.m11 + m.m12)

  @inline
  def toMat3 =
    Mat3(m00, m01, m02,
         m10, m11, m12,
         0,   0,   1)

  @inline
  def toMat4 =
    Mat4(m00, m01, m02, 0,
         m10, m11, m12, 0,
         0,   0,   1,   0,
         0,   0,   0,   1)

  def copy(m00: Float = m00, m01: Float = m01, m02: Float = m02,
           m10: Float = m10, m11: Float = m11, m12: Float = m12): Mat2d =
    Mat2d(m00, m01, m02,
          m10, m11, m12)

  override def toString =
    s"Mat2d(${m00}f,${m01}f,${m02}f,${m10}f,${m11}f,${m12}f)"

  override def equals(o: Any): Boolean = o match {
    case v: Mat2d =>
      m00 == v.m00 && m01 == v.m01 && m02 == v.m02 &&
      m10 == v.m10 && m11 == v.m11 && m12 == v.m12
    case _ => false
  }

  override def hashCode: Int =
    m00.hashCode() * 19 +  m01.hashCode() * 23 + m02.hashCode() * 29 +
    m10.hashCode() * 31 +  m11.hashCode() * 37 + m12.hashCode() * 41
}

object Mat2d {
  def apply(m00: Float, m01: Float, m02: Float,
            m10: Float, m11: Float, m12: Float): Mat2d =
    new Mat2d(m00, m01, m02,
              m10, m11, m12)

  def apply() = new Mat2d()

  @inline
  def scale2d(x: Float, y: Float): Mat2d =
    Mat2d(x, 0, 0,
          0, y, 0)

  @inline
  def scale2d(v: Vec2): Mat2d =
    scale2d(v.x, v.y)

  @inline
  def rotate2d(angle: Float): Mat2d = {
    val c = math.cos(angle).toFloat
    val s = math.sin(angle).toFloat
    Mat2d(c, -s, 0,
      s,  c, 0)
  }

  @inline
  def translate2d(x: Float, y: Float): Mat2d =
    Mat2d(1, 0, x,
          0, 1, y)

  @inline
  def translate2d(t: Vec2): Mat2d =
    translate2d(t.x, t.y)
}
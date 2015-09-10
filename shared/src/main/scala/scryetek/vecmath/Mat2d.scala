package scryetek.vecmath

/**
 * A 2x3 matrix for affine transforms in R<sup>2</sup>
 */
class Mat2d(var m0: Float,  var m1: Float,
            var m2: Float,  var m3: Float,
            var m4: Float,  var m5: Float) {

  /** Construct a new Mat2d initialized to the identity */
  def this() = this(1, 0,
                    0, 1,
                    0, 0)

  /** Construct a copy of a Mat2d. */
  def this(m: Mat2d) =
    this(m.m0, m.m1,
         m.m2, m.m3,
         m.m4, m.m5)

  /** Construct a copy of a Mat3 with the rightmost column removed */
  def this(m: Mat3) =
    this(m.m00, m.m01,
         m.m10, m.m11,
         m.m20, m.m21)

  def set(m0: Float, m1: Float,
          m2: Float, m3: Float,
          m4: Float, m5: Float): Mat2d = {
    this.m0 = m0; this.m1 = m1
    this.m2 = m2; this.m3 = m3
    this.m4 = m4; this.m5 = m5
    this
  }

  def set(m: Mat2d): Mat2d =
    this.set(m.m0, m.m1,
             m.m2, m.m3,
             m.m4, m.m5)

  def set(m: Mat3): Mat2d =
    this.set(m.m00, m.m01,
             m.m10, m.m11,
             m.m20, m.m21)

  /** Set this matrix to the identity */
  def setIdentity() =
    this.set(1, 0, 0, 1, 0, 0)

  /** Invert this matrix into another Mat2d */
  def invertInto(m: Mat2d): Mat2d = {
    var det = m0 * m3 - m1 * m2
    if(det == 0)
      return null
    det = 1/det
    m.set( m3 * det, -m1 * det,
          -m2 * det,  m0 * det,
          (m2 * m5 - m3 * m4) * det,
          (m1 * m4 - m0 * m5) * det)
  }

  /** Multiply a vector by this matrix */
  def *(v: Vec2): Vec2 =
    Vec2(v.x * m0 + v.y * m2 + m4,
         v.x * m1 + v.y * m3 + m5)

  /** Multiply this Mat2d by another Mat2d, creating a new Mat2d */
  def *(m: Mat2d): Mat2d =
    multiplyInto(Mat2d(m), m)

  /** Destructively invert this matrix */
  def invert(): Mat2d =
    invertInto(this)

  /** Compute the determinant of this matrix */
  def determinant: Float =
    m0 * m3 - m1 * m2

  /** Multiply this matrix by another matrix, into an output matrix */
  def multiplyInto(out: Mat2d, m: Mat2d): Mat2d = {
    out.set(m0 * m.m0 + m2*m.m1,
            m1 * m.m0 + m3*m.m1,
            m0 * m.m2 + m2*m.m3,
            m1 * m.m2 + m3*m.m3,
            m0 * m.m4 + m2*m.m5 + m4,
            m1 * m.m4 + m3*m.m5 + m5)
  }

  /** Destructively multiply this matrix with another */
  def multiply(m: Mat2d): Mat2d =
    multiplyInto(this, m)

  /** Rotate this matrix by the given angle, storing the result in an output matrix */
  def rotateInto(out: Mat2d, angle: Float): Mat2d = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    out.set(m0 *  c + m2 * s, m1 *  c + m3 * s,
            m0 * -s + m2 * c, m1 * -s + m3 * c,
            m4,               m5)
  }

  def rotate(angle: Float): Mat2d =
    rotateInto(this, angle)

  def scaleInto(out: Mat2d, v: Vec2): Mat2d =
    out.set(m0*v.x, m1*v.x, m2*v.y, m3*v.y, m4, m5)

  def scale(v: Vec2): Mat2d =
    scaleInto(this, v)

  def translateInto(out: Mat2d, v: Vec2): Mat2d =
    out.set(m0, m1, m2, m3, m0*v.x + m2*v.y + m4, m1*v.x + m3*v.y + m5)

  def translate(v: Vec2): Mat2d =
    translateInto(this, v)

  override def toString: String =
    s"Mat2d($m0, $m1, $m2,\n"+
    s"      $m3, $m4, $m5)"

  def toArray(array: Array[Float]): Array[Float] = {
    array(0) = m0; array(1) = m1; array(2) = m2
    array(3) = m3; array(4) = m4; array(5) = m5
    array
  }
}

object Mat2d {
  def apply(): Mat2d = new Mat2d()

  def apply(m0: Float, m1: Float,
            m2: Float, m3: Float,
            m4: Float, m5: Float): Mat2d =
    new Mat2d(m0, m1, m2, m3, m4, m5)

  def apply(m: Mat2d): Mat2d =
    new Mat2d(m)

  def apply(m: Mat3): Mat2d =
    new Mat2d(m)
}
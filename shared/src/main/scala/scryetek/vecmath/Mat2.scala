package scryetek.vecmath

final class Mat2(
  var m00: Float, var m01: Float,
  var m10: Float, var m11: Float) {
  @inline
  def set(
    m00: Float = this.m00, m01: Float = this.m01,
    m10: Float = this.m10, m11: Float = this.m11): Mat2 = {
    this.m00 = m00; this.m01 = m01
    this.m10 = m10; this.m11 = m11
    this
  }

  @inline
  def set(m: Mat2): Mat2 =
    set(m.m00, m.m01,
        m.m10, m.m11)

  @inline
  def this() = this(1, 0, 0, 1)

  @inline
  def determinant = m00*m11-m01*m10

  @inline
  def inverted = {
    var det = determinant
    assert(det != 0, "Matrix is not invertable")
    det = 1/det
    Mat2( m11*det, -m01*det,
         -m10*det,  m00*det)
  }

  @inline
  def invert(out: Mat2): Mat2 = {
    var det = determinant
    assert(det != 0, "Matrix is not invertable")
    det = 1/det
    out.set( m11*det, -m01*det,
            -m10*det,  m00*det)
  }

  /** Returns the transpose of this matrix. */
  @inline
  def transposed =
    Mat2(m00, m10,
         m01, m11)

  /** Transposes this matrix into the specified out matrix. */
  @inline
  def transpose(out: Mat2 = this): Mat2 =
    out.set(m00, m10,
            m01, m11)

  /** Returns the adjoint of this matrix. */
  @inline
  def adjointed =
    Mat2( m11, -m01,
         -m10,  m00)

  /** Puts the adjoint of this matrix into the specified out matrix. */
  @inline
  def adjoint(out: Mat2): Mat2 =
    out.set( m11, -m01,
            -m10, m00)


  /** Adds two matrices */
  @inline
  def +(m: Mat2): Mat2 =
    Mat2(m00 + m.m00, m01 + m.m01,
      m10 + m.m10, m11 + m.m11)

  /** Destructively add another matrix to this matrix into the output matrix. */
  @inline
  def add(m: Mat2, out: Mat2 = this): Mat2 =
    out.set(m00 + m.m00, m01 + m.m01,
            m10 + m.m10, m11 + m.m11)


  /** Adds two matrices */
  @inline
  def -(m: Mat2): Mat2 =
    Mat2(m00 - m.m00, m01 - m.m01,
         m10 - m.m10, m11 - m.m11)

  /** Destructively add another matrix to this matrix into the output matrix. */
  @inline
  def sub(m: Mat2, out: Mat2 = this): Mat2 =
    out.set(m00 - m.m00, m01 - m.m01,
            m10 - m.m10, m11 - m.m11)

  /** Transforms a 2-vector by this matrix. */
  @inline
  def *(v: Vec2): Vec2 =
    Vec2(m00*v.x + m01*v.y, m10*v.x + m11*v.y)

  /** Transforms a 2-vector into the specified out vector. */
  @inline
  def mul(v: Vec2, out: Vec2): Vec2 =
    out.set(m00*v.x + m01*v.y, m10*v.x + m11*v.y)

  /** Destructively transforms a 2-vector. */
  @inline
  def mul(v: Vec2): Vec2 =
    mul(v,v)

  /** Returns a scaled copy of this matrix. */
  @inline
  def *(s: Float): Mat2 =
    Mat2(m00*s, m01*s,
         m10*s, m11*s)

  @inline
  def scale(s: Float, out: Mat2 = this): Mat2 =
    out.set(m00*s, m01*s,
            m10*s, m11*s)

  /** Returns the result of multiplying this matrix by another matrix. */
  @inline
  def *(m: Mat2): Mat2 =
    Mat2(m00*m.m00 + m01*m.m10, m00*m.m01 + m01*m.m11,
         m10*m.m00 + m11*m.m10, m10*m.m01 + m11*m.m11)

  /** Copies the result of postmultiplying another matrix by this matrix into the specified output matrix. */
  @inline
  def postMultiply(m: Mat2, out: Mat2 = this): Mat2 =
    out.set(m00*m.m00 + m01*m.m10, m00*m.m01 + m01*m.m11,
            m10*m.m00 + m11*m.m10, m10*m.m01 + m11*m.m11)

  /** Copies the result of premultiplying another matrix by this matrix into the specified output matrix. */
  @inline
  def preMultiply(m: Mat2, out: Mat2 = this): Mat2 =
    out.set(m.m00*m00 + m.m01*m10, m.m00*m01 + m.m01*m11,
            m.m10*m00 + m.m11*m10, m.m10*m01 + m.m11*m11)

  def copy(m00: Float = m00, m01: Float = m01,
           m10: Float = m10, m11: Float = m11) =
    Mat2(m00, m01,
         m10, m11)

  override def toString =
    s"Mat2(${m00}f,${m01}f,${m10}f,${m11}f)"

  override def equals(o: Any): Boolean = o match {
    case v: Mat2 => m00 == v.m00 && m01 == v.m01 && m10 == v.m10 && m11 == v.m11
    case _ => false
  }

  override def hashCode: Int =
    m00.hashCode() * 19 +  m01.hashCode() * 23 +
    m10.hashCode() * 31 +  m11.hashCode() * 37
}

object Mat2 {
  def apply() = new Mat2()

  def apply(m00: Float, m01: Float, m10: Float, m11: Float) =
    new Mat2(m00, m01, m10, m11)

  @inline
  def rotate(angle: Float): Mat2 = {
    val c = math.cos(angle).toFloat
    val s = math.sin(angle).toFloat
    Mat2(c, -s,
         s,  c)
  }

  @inline
  def scale(x: Float, y: Float): Mat2 =
    Mat2(x, 0,
         0, y)

  @inline
  def scale(s: Vec2): Mat2 =
    scale(s.x, s.y)
}
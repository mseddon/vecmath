package scryetek.vecmath

/**
 * Created by Matt on 06/07/2014.
 */
case class Mat2(var m00: Float, var m01: Float,
                var m10: Float, var m11: Float) {
  def this() =
    this(1, 0,
         0, 1)

  def this(m: Mat2) =
    this(m.m00, m.m01,
         m.m10, m.m11)

  def set(m00: Float, m01: Float,
          m10: Float, m11: Float): Mat2 = {
    this.m00 = m00; this.m01 = m01
    this.m10 = m10; this.m11 = m11
    this
  }

  def set(m: Mat2): Mat2 = {
    this.m00 = m.m00; this.m01 = m.m01
    this.m10 = m.m10; this.m11 = m.m11
    this
  }

  def setIdentity: Mat2 = {
    m00 = 1; m01 = 0
    m10 = 0; m11 = 1
    this
  }

  def transposeInto(m: Mat2): Mat2 =
    m.set(m00, m10,
          m01, m11)

  def transpose: Mat2 =
    transposeInto(this)

  def invertInto(m: Mat2): Mat2 = {
    var det = m00*m11 - m10*m01

    if(det == 0)
      return null

    det = 1/det

    m.set( m11*det, -m01*det,
          -m10*det,  m00*det)
  }

  def invert: Mat2 =
    invertInto(this)

  def adjointInto(m: Mat2): Mat2 =
    m.set( m11, -m01,
          -m10,  m00)

  def adjoint: Mat2 =
    adjointInto(this)

  def determinant: Float = m00*m11 - m10*m01

  def multiplyInto(out: Mat2, m: Mat2): Mat2 =
    out.set(m00*m.m00 + m10*m.m01,
            m01*m.m00 + m11*m.m01,
            m00*m.m10 + m10*m.m11,
            m01*m.m10 + m11*m.m11)

  def multiply(m: Mat2): Mat2 =
    multiplyInto(this, m)

  def rotateInto(out: Mat2, angle: Float): Mat2 = {
    val c = Math.cos(angle).toFloat
    val s = Math.sin(angle).toFloat
    out.set(m00 *  c + m10 * s, m01 *  c + m11 * s,
            m00 * -s + m10 * c, m01 * -s + m11 * c)
  }

  def rotate(angle: Float): Mat2 =
    rotateInto(this, angle)

  def scaleInto(out: Mat2, v: Vec2): Mat2 =
    out.set(m00*v.x, m01*v.x,
            m10*v.y, m11*v.y)

  def scale(v: Vec2): Mat2 =
    scaleInto(this, v)

  override def toString: String =
    s"Mat2($m00, $m01,\n" +
    s"     $m10, $m11)"

  def toArray(array: Array[Float]): Array[Float] = {
    array(0)  = m00; array(1)  = m01
    array(2)  = m10; array(3)  = m11
    array
  }
}

object Mat2 {
  def apply(): Mat2 = new Mat2()

  def apply(m: Mat2): Mat2 = new Mat2(m)
}
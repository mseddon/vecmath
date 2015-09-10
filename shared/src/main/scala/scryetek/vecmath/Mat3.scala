package scryetek.vecmath

/**
 * Created by Matt on 06/07/2014.
 */
class Mat3(var m00: Float, var m01: Float, var m02: Float,
           var m10: Float, var m11: Float, var m12: Float,
           var m20: Float, var m21: Float, var m22: Float) {
  def this() =
    this(1, 0, 0,
         0, 1, 0,
         0, 0, 1)

  def this(m: Mat3) =
    this(m.m00, m.m01, m.m02,
         m.m10, m.m11, m.m12,
         m.m20, m.m21, m.m22)

  def this(m: Mat4) =
    this(m.m00, m.m01, m.m02,
         m.m10, m.m11, m.m12,
         m.m20, m.m21, m.m22)

  def set(m00: Float, m01: Float, m02: Float,
          m10: Float, m11: Float, m12: Float,
          m20: Float, m21: Float, m22: Float): Mat3 = {
    this.m00 = m00; this.m01 = m01; this.m02 = m02
    this.m10 = m10; this.m11 = m11; this.m12 = m12
    this.m20 = m20; this.m21 = m21; this.m22 = m22
    this
  }

  def set(m: Mat3): Mat3 = {
    m00 = m.m00; m01 = m.m01; m02 = m.m02
    m10 = m.m10; m11 = m.m11; m12 = m.m12
    m20 = m.m20; m21 = m.m21; m22 = m.m22
    this
  }

  def set(m: Mat4): Mat3 = {
    m00 = m.m00; m01 = m.m01; m02 = m.m02
    m10 = m.m10; m11 = m.m11; m12 = m.m12
    m20 = m.m20; m21 = m.m21; m22 = m.m22
    this
  }

  def *(v: Vec3): Vec3 =
    Vec3(v.x * m00 + v.y * m10 + v.z * m20,
         v.x * m01 + v.y * m11 + v.z * m21,
         v.x * m02 + v.y * m12 + v.z * m22)

  def *(v: Vec2): Vec2 = {
    val s = v.x * m02 + v.y * m12 + m22
    Vec2((v.x * m00 + v.y * m10 + m20) / s,
         (v.x * m01 + v.y * m11 + m21) / s)
  }

  def setIdentity: Mat3 = {
    m00 = 1; m01 = 0; m02 = 0
    m10 = 0; m11 = 1; m12 = 0
    m20 = 0; m21 = 0; m22 = 1
    this
  }

  def transposeInto(m: Mat3): Mat3 =
    m.set(m00, m10, m20,
          m01, m11, m21,
          m02, m12, m22)

  def transpose: Mat3 =
    transposeInto(this)

  def invertInto(m: Mat3): Mat3 = {
    val b01 =  m22*m11 - m12*m21
    val b11 = -m22*m10 + m12*m20
    val b21 =  m21*m10 - m11*m20

    var det = m00 * b01 + m01 * b11 + m02 * b21
    if(det == 0)
      return null
    det = 1.0f / det

    m.set(b01*det, (-m22*m01 + m02*m21) * det, ( m12*m01 - m02*m11)*det,
          b11*det, ( m22*m00 - m02*m20) * det, (-m12*m00 + m02*m10)*det,
          b21*det, (-m21*m00 + m01*m20) * det, ( m11*m00 - m01*m10)*det)
  }

  def invert: Mat3 = invertInto(this)

  def adjointInto(m: Mat3): Mat3 =
    m.set(m11*m22 - m12*m21, m02*m21 - m01*m22, m01*m12 - m02*m11,
          m12*m20 - m10*m22, m00*m22 - m02*m20, m02*m10 - m00*m12,
          m10*m21 - m11*m20, m01*m20 - m00*m21, m00*m11 - m01*m10)

  def adjoint: Mat3 = adjointInto(this)
  
  def determinant: Float =
    m00*( m22*m11 - m12*m21) +
    m01*(-m22*m10 + m12*m20) +
    m02*( m21*m10 - m11*m20)
  
  def multiplyInto(out: Mat3, m: Mat3): Mat3 =
    out.set(m.m00*m00 + m.m01*m10 + m.m02*m20,
            m.m00*m01 + m.m01*m11 + m.m02*m21,
            m.m00*m02 + m.m01*m12 + m.m02*m22,

            m.m10*m00 + m.m11*m10 + m.m12*m20,
            m.m10*m01 + m.m11*m11 + m.m12*m21,
            m.m10*m02 + m.m11*m12 + m.m12*m22,

            m.m20*m00 + m.m21*m10 + m.m22*m20,
            m.m20*m01 + m.m21*m11 + m.m22*m21,
            m.m20*m02 + m.m21*m12 + m.m22*m22)

  def multiply(m: Mat3): Mat3 =
    multiplyInto(this, m)

  def translateInto(out: Mat3, v: Vec2): Mat3 =
    out.set(m00, m01, m02,
            m10, m11, m12,
            v.x*m00 + v.y*m10 + m20,
            v.x*m01 + v.y*m11 + m21,
            v.x*m02 + v.y*m12 + m22)

  def translate(v: Vec2): Mat3 =
    translateInto(this, v)

  def rotateInto(out: Mat3, angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    out.set(c*m00 + s*m10, c*m01 + s*m11, c*m02 + s*m12,
            c*m10 - s*m00, c*m11 - s*m01, c*m12 - s*m02,
            m20,           m21,           m22)
  }

  def rotate(angle: Float): Mat3 =
    rotateInto(this, angle)

  def scaleInto(out: Mat3, v: Vec2): Mat3 =
    out.set(v.x*m00, v.x*m01, v.x*m02,
            v.y*m10, v.y*m11, v.y*m12,
            m20,     m21,     m22)

  def scale(v: Vec2): Mat3 =
    scaleInto(this, v)

  def set(q: Quat): Mat3 = {
    val x2 = q.x + q.x
    val y2 = q.y + q.y
    val z2 = q.z + q.z

    val xx = q.x*x2
    val yx = q.y*x2
    val yy = q.y*y2
    val zx = q.z*x2
    val zy = q.z*y2
    val zz = q.z*z2
    val wx = q.w*x2
    val wy = q.w*y2
    val wz = q.w*z2

    set(1 - yy - zz, yx + wz,     zx - wy,
        yx - wz,     1 - xx -zz,  zy + wx,
        zx + wy,     1 - xx - zz, zy + wx)
  }

  // fromMat2d

  override def toString: String =
    s"Mat3($m00, $m01, $m02,\n" +
    s"     $m10, $m11, $m12,\n" +
    s"     $m20, $m21, $m22)"

  def toArray(array: Array[Float]): Array[Float] = {
    array(0)  = m00; array(1)  = m01; array(2)  = m02
    array(3)  = m10; array(4)  = m11; array(5)  = m12
    array(6)  = m20; array(7)  = m21; array(8)  = m22
    array
  }
}

object Mat3 {
  def apply(): Mat3 = new Mat3()

  def apply(m00: Float, m01: Float, m02: Float,
            m10: Float, m11: Float, m12: Float,
            m20: Float, m21: Float, m22: Float): Mat3 =
    new Mat3(m00, m01, m02,
             m10, m11, m12,
             m20, m21, m22)

  def apply(m: Mat3): Mat3 = new Mat3(m)
  def apply(m: Mat4): Mat3 = new Mat3(m)
}
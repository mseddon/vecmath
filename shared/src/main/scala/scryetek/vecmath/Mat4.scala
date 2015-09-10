package scryetek.vecmath

class Mat4(var m00: Float, var m01: Float, var m02: Float, var m03: Float,
           var m10: Float, var m11: Float, var m12: Float, var m13: Float,
           var m20: Float, var m21: Float, var m22: Float, var m23: Float,
           var m30: Float, var m31: Float, var m32: Float, var m33: Float) {

  def this() = this(1, 0, 0, 0,
                    0, 1, 0, 0,
                    0, 0, 1, 0,
                    0, 0, 0, 1)

  def this(m: Mat4) =
    this(m.m00, m.m01, m.m02, m.m03,
         m.m10, m.m11, m.m12, m.m13,
         m.m20, m.m21, m.m22, m.m23,
         m.m30, m.m31, m.m32, m.m33)

  def set(m00: Float, m01: Float, m02: Float, m03: Float,
          m10: Float, m11: Float, m12: Float, m13: Float,
          m20: Float, m21: Float, m22: Float, m23: Float,
          m30: Float, m31: Float, m32: Float, m33: Float): Mat4 = {
    this.m00 = m00; this.m01 = m01; this.m02 = m02; this.m03 = m03
    this.m10 = m10; this.m11 = m11; this.m12 = m12; this.m13 = m13
    this.m20 = m20; this.m21 = m21; this.m22 = m22; this.m23 = m23
    this.m30 = m30; this.m31 = m31; this.m32 = m32; this.m33 = m33
    this
  }

  def set(m: Mat4): Mat4 = {
    m00 = m.m00; m01 = m.m01; m02 = m.m02; m03 = m.m03
    m10 = m.m10; m11 = m.m11; m12 = m.m12; m13 = m.m13
    m20 = m.m20; m21 = m.m21; m22 = m.m22; m23 = m.m23
    m30 = m.m30; m31 = m.m31; m32 = m.m32; m33 = m.m33
    this
  }

  def setIdentity: Mat4 = {
    m00 = 1; m01 = 0; m02 = 0; m03 = 0
    m10 = 0; m11 = 1; m12 = 0; m13 = 0
    m20 = 0; m21 = 0; m22 = 1; m23 = 0
    m30 = 0; m31 = 0; m32 = 0; m33 = 1
    this
  }

  def transposeInto(m: Mat4): Mat4 =
    m.set(m00, m10, m20, m30,
          m01, m11, m21, m31,
          m02, m12, m22, m32,
          m03, m13, m33, m33)

  def transpose: Mat4 =
    transposeInto(this)

  def invertInto(m: Mat4): Mat4 = {
    val b00 = m00*m11 - m01*m10
    val b01 = m00*m12 - m02*m10
    val b02 = m00*m13 - m03*m10
    val b03 = m01*m12 - m02*m11
    val b04 = m01*m13 - m03*m11
    val b05 = m02*m13 - m03*m12
    val b06 = m20*m31 - m21*m30
    val b07 = m20*m32 - m22*m30
    val b08 = m20*m33 - m23*m30
    val b09 = m21*m32 - m22*m31
    val b10 = m21*m33 - m23*m31
    val b11 = m22*m33 - m23*m32

    var det = b00*b11 - b01*b10 + b02*b09 + b03*b08 - b04*b07 + b05*b06
    if(det == 0)
      return null
    det = 1.0f / det

    m.set((m11*b11 - m12*b10 + m13*b09)*det,
          (m02*b10 - m01*b11 - m03*b09)*det,
          (m31*b05 - m32*b04 + m33*b03)*det,
          (m22*b04 - m21*b05 - m23*b03)*det,
          (m12*b08 - m10*b11 - m13*b07)*det,
          (m00*b11 - m02*b08 + m03*b07)*det,
          (m32*b02 - m30*b05 - m33*b01)*det,
          (m20*b05 - m22*b02 + m23*b01)*det,
          (m10*b10 - m11*b08 + m13*b06)*det,
          (m01*b08 - m00*b10 - m03*b06)*det,
          (m30*b04 - m31*b02 + m33*b00)*det,
          (m21*b02 - m20*b04 - m23*b00)*det,
          (m11*b07 - m10*b09 - m12*b06)*det,
          (m00*b09 - m01*b07 + m02*b06)*det,
          (m31*b01 - m30*b03 - m32*b00)*det,
          (m20*b03 - m21*b01 + m22*b00)*det)
  }

  def invert: Mat4 =
    invertInto(this)

  def adjointInto(m: Mat4): Mat4 =
    m.set(  m11*(m22*m33 - m23*m32) - m21*(m12*m33 - m13*m32) + m31*(m12*m23 - m13*m22),
          -(m01*(m22*m33 - m23*m32) - m21*(m02*m33 - m03*m32) + m31*(m02*m23 - m03*m22)),
            m01*(m12*m33 - m13*m32) - m11*(m02*m33 - m03*m32) + m31*(m02*m13 - m03*m12),
          -(m01*(m12*m23 - m13*m22) - m11*(m02*m23 - m03*m22) + m21*(m02*m13 - m03*m12)),
          -(m10*(m22*m33 - m23*m32) - m20*(m12*m33 - m13*m32) + m30*(m12*m23 - m13*m22)),
            m00*(m22*m33 - m23*m32) - m20*(m02*m33 - m03*m32) + m30*(m02*m23 - m03*m22),
          -(m00*(m12*m33 - m13*m32) - m10*(m02*m33 - m03*m32) + m30*(m02*m13 - m03*m12)),
            m00*(m12*m23 - m13*m22) - m10*(m02*m23 - m03*m22) + m20*(m02*m13 - m03*m12),
            m10*(m21*m33 - m23*m31) - m20*(m11*m33 - m13*m31) + m30*(m11*m23 - m13*m21),
          -(m00*(m21*m33 - m23*m31) - m20*(m01*m33 - m03*m31) + m30*(m01*m23 - m03*m21)),
            m00*(m11*m33 - m13*m31) - m10*(m01*m33 - m03*m31) + m30*(m01*m13 - m03*m11),
          -(m00*(m11*m23 - m13*m21) - m10*(m01*m23 - m03*m21) + m20*(m01*m13 - m03*m11)),
          -(m10*(m21*m32 - m22*m31) - m20*(m11*m32 - m12*m31) + m30*(m11*m22 - m12*m21)),
            m00*(m21*m32 - m22*m31) - m20*(m01*m32 - m02*m31) + m30*(m01*m22 - m02*m21),
          -(m00*(m11*m32 - m12*m31) - m10*(m01*m32 - m02*m31) + m30*(m01*m12 - m02*m11)),
            m00*(m11*m22 - m12*m21) - m10*(m01*m22 - m02*m21) + m20*(m01*m12 - m02*m11))

  def adjoint: Mat4 =
    adjointInto(this)

  def determinant: Float = {
    val b00 = m00*m11 - m01*m10
    val b01 = m00*m12 - m02*m10
    val b02 = m00*m13 - m03*m10
    val b03 = m01*m12 - m02*m11
    val b04 = m01*m13 - m03*m11
    val b05 = m02*m13 - m03*m12
    val b06 = m20*m31 - m21*m30
    val b07 = m20*m32 - m22*m30
    val b08 = m20*m33 - m23*m30
    val b09 = m21*m32 - m22*m31
    val b10 = m21*m33 - m23*m31
    val b11 = m22*m33 - m23*m32

    b00*b11 - b01*b10 + b02*b09 + b03*b08 - b04*b07 + b05*b06
  }

  def multiplyInto(out: Mat4, m: Mat4): Mat4 =
    out.set(m.m00*m00 + m.m01*m10 + m.m02*m20 + m.m03*m30,
            m.m00*m01 + m.m01*m11 + m.m02*m21 + m.m03*m31,
            m.m00*m02 + m.m01*m12 + m.m02*m22 + m.m03*m32,
            m.m00*m03 + m.m01*m13 + m.m02*m23 + m.m03*m33,
            m.m10*m00 + m.m11*m10 + m.m12*m20 + m.m13*m30,
            m.m10*m01 + m.m11*m11 + m.m12*m21 + m.m13*m31,
            m.m10*m02 + m.m11*m12 + m.m12*m22 + m.m13*m32,
            m.m10*m03 + m.m11*m13 + m.m12*m23 + m.m13*m33,
            m.m20*m00 + m.m21*m10 + m.m22*m20 + m.m23*m30,
            m.m20*m01 + m.m21*m11 + m.m22*m21 + m.m23*m31,
            m.m20*m02 + m.m21*m12 + m.m22*m22 + m.m23*m32,
            m.m20*m03 + m.m21*m13 + m.m22*m23 + m.m23*m33,
            m.m30*m00 + m.m31*m10 + m.m32*m20 + m.m33*m30,
            m.m30*m01 + m.m31*m11 + m.m32*m21 + m.m33*m31,
            m.m30*m02 + m.m31*m12 + m.m32*m22 + m.m33*m32,
            m.m30*m03 + m.m31*m13 + m.m32*m23 + m.m33*m33)

  def multiplyInto(out: Mat4,
                   b00: Float, b01: Float, b02: Float, b03: Float,
                   b10: Float, b11: Float, b12: Float, b13: Float,
                   b20: Float, b21: Float, b22: Float, b23: Float,
                   b30: Float, b31: Float, b32: Float, b33: Float) =
    out.set(b00*m00 + b01*m10 + b02*m20 + b03*m30,
            b00*m01 + b01*m11 + b02*m21 + b03*m31,
            b00*m02 + b01*m12 + b02*m22 + b03*m32,
            b00*m03 + b01*m13 + b02*m23 + b03*m33,
            b10*m00 + b11*m10 + b12*m20 + b13*m30,
            b10*m01 + b11*m11 + b12*m21 + b13*m31,
            b10*m02 + b11*m12 + b12*m22 + b13*m32,
            b10*m03 + b11*m13 + b12*m23 + b13*m33,
            b20*m00 + b21*m10 + b22*m20 + b23*m30,
            b20*m01 + b21*m11 + b22*m21 + b23*m31,
            b20*m02 + b21*m12 + b22*m22 + b23*m32,
            b20*m03 + b21*m13 + b22*m23 + b23*m33,
            b30*m00 + b31*m10 + b32*m20 + b33*m30,
            b30*m01 + b31*m11 + b32*m21 + b33*m31,
            b30*m02 + b31*m12 + b32*m22 + b33*m32,
            b30*m03 + b31*m13 + b32*m23 + b33*m33)

  def multiply(m: Mat4): Mat4 =
    multiplyInto(this, m)

  def translateInto(out: Mat4, v: Vec3): Mat4 = {
    val x = v.x; val y = v.y; val z = v.z
    out.set(m00, m01, m02, m03,
            m10, m11, m12, m13,
            m20, m21, m22, m23,
            m00*x + m10*y + m20*z + m30,
            m01*x + m11*y + m21*z + m31,
            m02*x + m12*y + m22*z + m32,
            m03*x + m13*y + m23*z + m33)
  }

  def translate(v: Vec3): Mat4 =
    translateInto(this, v)

  def scaleInto(m: Mat4, v: Vec3): Mat4 = {
    val x = v.x; val y = v.y; val z = v.z
    m.set(m00*x, m01*x, m02*x, m03*x,
          m10*y, m11*y, m12*y, m13*y,
          m20*z, m21*z, m22*z, m23*z,
          m30,     m01,     m02,     m03)
  }

  def scale(v: Vec3): Mat4 =
    scaleInto(this, v)

  def rotateInto(m: Mat4, angle: Float, axis: Vec3): Mat4 = {
    val axis2 = new Vec3(axis).normalize
    val x = axis2.x; val y = axis2.y; val z = axis2.z
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    val t = 1-c

    val b00 = x*x*t + c;     val b01 = y*x*t + z*s; val b02 = z*x*t - y*s
    val b10 = x*y*t - z*s; val b11 = y*y*t + c;     val b12 = z*y*t + x*s
    val b20 = x*z*t + y*s; val b21 = y*z*t - x*s; val b22 = z*z*t + c

    m.set(m00*b00 + m10*b01 + m20*b02,
          m01*b00 + m11*b01 + m21*b02,
          m02*b00 + m12*b01 + m22*b02,
          m03*b00 + m13*b01 + m23*b02,

          m00*b10 + m10*b11 + m20*b12,
          m01*b10 + m11*b11 + m21*b12,
          m02*b10 + m12*b11 + m22*b12,
          m03*b10 + m13*b11 + m23*b12,

          m00*b20 + m10*b21 + m20*b22,
          m01*b20 + m11*b21 + m21*b22,
          m02*b20 + m12*b21 + m22*b22,
          m03*b20 + m13*b21 + m23*b22,

          m30, m31, m32, m33)
  }

  def rotate(angle: Float, axis: Vec3): Mat4 =
    rotateInto(this, angle, axis)

  def rotateXInto(m: Mat4, angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat

    m.set(m00,           m01,             m02,           m03,
          m10*c + m20*s, m11*c + m21*s,   m12*c + m22*s, m13*c + m23*s,
          m20*c - m10*s, m21*c - m11*s,   m22*c - m12*s, m23*c - m13*s,
          m30,           m31,             m33,           m33)
  }

  def rotateX(angle: Float): Mat4 =
    rotateXInto(this, angle)

  def rotateYInto(m: Mat4, angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat

    m.set(m00*c - m20*s, m01*c - m21*s, m02*c - m22*s, m03*c - m23*s,
          m10,           m11,           m12,           m13,
          m00*s + m20*c, m01*s + m21*c, m02*s + m22*c, m03*s + m23*c,
          m30,           m31,           m32,           m33)
  }

  def rotateY(angle: Float): Mat4 =
    rotateYInto(this, angle)

  def rotateZInto(m: Mat4, angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat

    m.set(m00*c + m10*s, m01*c + m11*s, m20*s + m12*s, m03*c + m13*s,
          m10*c - m00*s, m11*c - m01*s, m12*c - m02*s, m13*c - m03*s,
          m20,           m21,           m22,           m23,
          m30,           m31,           m32,           m33)
  }

  def rotateZ(angle: Float): Mat4 =
    rotateZInto(this, angle)

  /*
  def fromRotationTranslation
  */

  def set(q: Quat): Mat4 = {
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

    set(1 - yy - zz, yx + wz,     zx - wy, 0,
        yx - wz,     1 - xx -zz,  zy + wx, 0,
        zx + wy,     1 - xx - zz, zy + wx, 0,
        0,           0,           0,       1)
  }

  def lookAt(eye: Vec3, center: Vec3, up: Vec3): Mat4 = {
    val z = new Vec3(eye).sub(center).normalize
    val x = new Vec3(up).cross(z).normalize
    val y = new Vec3(z).cross(x).normalize

    set( x.x,        y.x,      z.x,     0,
         x.y,        y.y,      z.y,     0,
         x.z,        y.z,      z.z,     0,
        -(x*eye), -(y*eye), -(z*eye), 1)
  }

  def frustum(left: Float, right: Float, bottom: Float, top: Float, near: Float, far: Float): Mat4 = {
    val rl = 1 / (right-left)
    val tb = 1 / (top-bottom)
    val nf = 1 / (near-far)

    set((near*2)*rl,     0,               0,              0,
        0,               (near*2)*tb,     0             , 0,
        (right+left)*rl, (top+bottom)*tb, (far+near)*nf, -1,
        -1,              0,               (far*near)*nf,  0)
  }

  def perspective(fovy: Float, aspect: Float, near: Float, far: Float): Mat4 = {
    val f = 1.0f / Math.tan(fovy / 2).toFloat
    val nf = 1 / (near-far)

    set(f/aspect, 0,                   0,                0,
        0,        f,                   0,                0,
        0,        0,                   (far + near)*nf, -1,
        0,        0,                   2*(far*near)*nf,  0)
  }

  def ortho(left: Float, right: Float, bottom: Float, top: Float, near: Float, far: Float): Mat4 = {
    val lr = 1 / (left - right)
    val bt = 1 / (bottom - top)
    val nf = 1 / (near - far)

    set(-2 * lr,             0,                   0,                0,
        0,                  -2 * bt,              0,                0,
        0,                   0,                   2 * nf,           0,
        (left + right) * lr, (top + bottom) * bt, (far + near) * nf, 1)
  }

  def inverseTransposeInto(out: Mat3): Mat3 = {
    val b00 = m00*m11 - m01*m10
    val b01 = m00*m12 - m02*m10
    val b02 = m00*m13 - m03*m10
    val b03 = m01*m12 - m02*m11
    val b04 = m01*m13 - m03*m11
    val b05 = m02*m13 - m03*m12
    val b06 = m20*m31 - m21*m30
    val b07 = m20*m32 - m22*m30
    val b08 = m20*m33 - m23*m30
    val b09 = m21*m32 - m22*m31
    val b10 = m21*m33 - m23*m31
    val b11 = m22*m33 - m23*m32

    var det = b00*b11 - b01*b10 + b02*b09 + b03*b08 - b04*b07 + b05*b06
    if(det == 0)
      return null
    det = 1/det

    out.set((m11*b11 - m12*b10 + m13*b09)*det,
            (m12*b08 - m10*b11 - m13*b07)*det,
            (m10*b10 - m11*b08 + m13*b06)*det,
    
            (m02*b10 - m01*b11 - m03*b09)*det,
            (m00*b11 - m02*b08 + m03*b07)*det,
            (m01*b08 - m00*b10 - m03*b06)*det,

            (m31*b05 - m32*b04 + m33*b03)*det,
            (m32*b02 - m30*b05 - m33*b01)*det,
            (m30*b04 - m31*b02 + m33*b00)*det)
  }

  override def toString: String =
    s"Mat4($m00, $m01, $m02, $m03,\n" +
    s"     $m10, $m11, $m12, $m13,\n" +
    s"     $m20, $m21, $m22, $m23,\n" +
    s"     $m30, $m31, $m32, $m33)"

  def toArray(array: Array[Float]): Array[Float] = {
    array(0)  = m00; array(1)  = m01; array(2)  = m02; array(3)  = m03
    array(4)  = m10; array(5)  = m11; array(6)  = m12; array(7)  = m13
    array(8)  = m20; array(9)  = m21; array(10) = m22; array(11) = m23
    array(12) = m30; array(13) = m31; array(14) = m32; array(15) = m33
    array
  }
}

object Mat4 {
  def apply(): Mat4 = new Mat4()

  def apply(m00: Float, m01: Float, m02: Float, m03: Float,
            m10: Float, m11: Float, m12: Float, m13: Float,
            m20: Float, m21: Float, m22: Float, m23: Float,
            m30: Float, m31: Float, m32: Float, m33: Float): Mat4 =
    new Mat4(m00, m01, m02, m03,
             m10, m11, m12, m13,
             m20, m21, m22, m23,
             m30, m31, m32, m33)

  def apply(m: Mat4): Mat4 = new Mat4(m)
}
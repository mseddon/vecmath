package scryetek.vecmath

final class Mat3(
  var m00: Float, var m01: Float, var m02: Float,
  var m10: Float, var m11: Float, var m12: Float,
  var m20: Float, var m21: Float, var m22: Float
) {
  @inline
  def set(m00: Float = m00, m01: Float = m01, m02: Float = m02,
          m10: Float = m10, m11: Float = m11, m12: Float = m12,
          m20: Float = m20, m21: Float = m22, m22: Float = m22): Mat3 = {
    this.m00 = m00; this.m01 = m01; this.m02 = m02
    this.m10 = m10; this.m11 = m11; this.m12 = m12
    this.m20 = m20; this.m21 = m21; this.m22 = m22
    this
  }

  @inline
  def set(m: Mat3): Mat3 =
    set(m.m00, m.m01, m.m02,
        m.m10, m.m11, m.m12,
        m.m20, m.m21, m.m22)

  @inline
  def this() =
    this(1, 0, 0,
         0, 1, 0,
         0, 0, 1)

  @inline
  def transposed =
    Mat3(m00, m10, m20,
      m01, m11, m21,
      m02, m12, m22)

  @inline
  def transpose(out: Mat3 = this): Mat3 =
    out.set(m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22)

  /** Adds two matrices */
  @inline
  def +(m: Mat3): Mat3 =
    Mat3(m00 + m.m00, m01 + m.m01, m02 + m.m02,
         m10 + m.m10, m11 + m.m11, m12 + m.m12,
         m20 + m.m20, m21 + m.m21, m22 + m.m22)

  /** Destructively add another matrix to this matrix into the output matrix. */
  @inline
  def add(m: Mat3, out: Mat3 = this): Mat3 =
    out.set(m00 + m.m00, m01 + m.m01, m02 + m.m02,
            m10 + m.m10, m11 + m.m11, m12 + m.m12,
            m20 + m.m20, m21 + m.m21, m22 + m.m22)

  /** Subtracts two matrices */
  @inline
  def -(m: Mat3): Mat3 =
    Mat3(m00 - m.m00, m01 - m.m01, m02 - m.m02,
         m10 - m.m10, m11 - m.m11, m12 - m.m12,
         m20 - m.m20, m21 - m.m21, m22 - m.m22)

  /** Destructively add another matrix to this matrix into the output matrix. */
  @inline
  def sub(m: Mat3, out: Mat3 = this): Mat3 =
    out.set(m00 - m.m00, m01 - m.m01, m02 - m.m02,
            m10 - m.m10, m11 - m.m11, m12 - m.m12,
            m20 - m.m20, m21 - m.m21, m22 - m.m22)

  @inline
  def *(v: Vec3): Vec3 =
    Vec3(
      m00*v.x + m01*v.y + m02*v.z,
      m10*v.x + m11*v.y + m12*v.z,
      m20*v.x + m21*v.y + m22*v.z)

  @inline
  def mul(v: Vec3, out: Vec3): Vec3 =
    out.set(m00*v.x + m01*v.y + m02*v.z,
            m10*v.x + m11*v.y + m12*v.z,
            m20*v.x + m21*v.y + m22*v.z)

  @inline
  def mul(v: Vec3): Vec3 =
    mul(v, v)

  @inline
  def *(s: Float): Mat3 =
    Mat3(m00*s, m01*s, m02*s,
      m10*s, m11*s, m12*s,
      m20*s, m21*s, m22*s)

  @inline
  def scale(s: Float, out: Mat3 = this): Mat3 =
    out.set(m00*s, m01*s, m02*s,
            m10*s, m11*s, m12*s,
            m20*s, m21*s, m22*s)

  @inline
  def *(b: Mat3): Mat3 =
    Mat3(b.m00*m00 + b.m10*m01 + b.m20*m02,
         b.m01*m00 + b.m11*m01 + b.m21*m02,
         b.m02*m00 + b.m12*m01 + b.m22*m02,

         b.m00*m10 + b.m10*m11 + b.m20*m12,
         b.m01*m10 + b.m11*m11 + b.m21*m12,
         b.m02*m10 + b.m12*m11 + b.m22*m12,

         b.m00*m20 + b.m10*m21 + b.m20*m22,
         b.m01*m20 + b.m11*m21 + b.m21*m22,
         b.m02*m20 + b.m12*m21 + b.m22*m22)

  @inline
  def *(q: Quat): Mat3 = {
    val xx = q.x*q.x; val xy = q.x*q.y; val xz = q.x*q.z; val xw = q.x*q.w; val ww = q.w*q.w
    val yy = q.y*q.y; val yz = q.y*q.z; val yw = q.y*q.w; val zz = q.z*q.z; val zw = q.z*q.w

    val b00 = 1-2*yy-2*zz; val b01 = 2*(xy-zw);   val b02 = 2*(xz+yw)
    val b10 = 2*(xy+zw);   val b11 = 1-2*xx-2*zz; val b12 = 2*(yz-xw)
    val b20 = 2*(xz-yw);   val b21 = 2*(yz+xw);   val b22 = 1-2*xx-2*yy

    Mat3(b00*m00 + b10*m01 + b20*m02,
         b01*m00 + b11*m01 + b21*m02,
         b02*m00 + b12*m01 + b22*m02,

         b00*m10 + b10*m11 + b20*m12,
         b01*m10 + b11*m11 + b21*m12,
         b02*m10 + b12*m11 + b22*m12,

         b00*m20 + b10*m21 + b20*m22,
         b01*m20 + b11*m21 + b21*m22,
         b02*m20 + b12*m21 + b22*m22)
  }
  @inline
  def postMultiply(m: Mat3): Mat3 =
    postMultiply(m.m00, m.m01, m.m02,
                 m.m10, m.m11, m.m12,
                 m.m20, m.m21, m.m22)

  @inline
  def postMultiply(m: Mat3, out: Mat3): Mat3 =
    postMultiply(m.m00, m.m01, m.m02,
                 m.m10, m.m11, m.m12,
                 m.m20, m.m21, m.m22, out)

  @inline
  def postMultiply(b00: Float, b01: Float, b02: Float,
                   b10: Float, b11: Float, b12: Float,
                   b20: Float, b21: Float, b22: Float,
                   out: Mat3 = this): Mat3 =
    out.set(b00*m00 + b10*m01 + b20*m02,
            b01*m00 + b11*m01 + b21*m02,
            b02*m00 + b12*m01 + b22*m02,

            b00*m10 + b10*m11 + b20*m12,
            b01*m10 + b11*m11 + b21*m12,
            b02*m10 + b12*m11 + b22*m12,

            b00*m20 + b10*m21 + b20*m22,
            b01*m20 + b11*m21 + b21*m22,
            b02*m20 + b12*m21 + b22*m22)

  @inline
  def preMultiply(m: Mat3, out: Mat3): Mat3 =
    preMultiply(m.m00, m.m01, m.m02,
                m.m10, m.m11, m.m12,
                m.m20, m.m21, m.m22, out)

  @inline
  def preMultiply(m: Mat3): Mat3 =
    preMultiply(m.m00, m.m01, m.m02,
                m.m10, m.m11, m.m12,
                m.m20, m.m21, m.m22, this)

  @inline
  def preMultiply(b00: Float, b01: Float, b02: Float,
                  b10: Float, b11: Float, b12: Float,
                  b20: Float, b21: Float, b22: Float,
                  out: Mat3 = this): Mat3 =
    out.set(m00*b00 + m10*b01 + m20*b02,
            m01*b00 + m11*b01 + m21*b02,
            m02*b00 + m12*b01 + m22*b02,

            m00*b10 + m10*b11 + m20*b12,
            m01*b10 + m11*b11 + m21*b12,
            m02*b10 + m12*b11 + m22*b12,

            m00*b20 + m10*b21 + m20*b22,
            m01*b20 + m11*b21 + m21*b22,
            m02*b20 + m12*b21 + m22*b22)

  @inline
  def preRotate(angle: Float, x: Float, y: Float, z: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    val t = 1-c

    preMultiply(x*x*t + c,   x*y*t - z*s, x*z*t+y*s,
                y*x*t + z*s, y*y*t + c,   y*z*t-x*s,
                x*z*t - y*s, y*z*t + x*s, z*z*t + c)
  }

  @inline
  def preRotate(angle: Float, axis: Vec3): Mat3 =
    preRotate(angle, axis.x, axis.y, axis.z)

  @inline
  def postRotate(angle: Float, x: Float, y: Float, z: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    val t = 1-c

    postMultiply(x*x*t + c,   x*y*t - z*s, x*z*t+y*s,
                 y*x*t + z*s, y*y*t + c,   y*z*t-x*s,
                 x*z*t - y*s, y*z*t + x*s, z*z*t + c)
  }

  @inline
  def postRotate(angle: Float, axis: Vec3): Mat3 =
    postRotate(angle, axis.x, axis.y, axis.z)

  @inline
  def preRotateQuat(x: Float, y: Float, z: Float, w: Float): Mat3 = {
    val xx = x*x; val xy = x*y; val xz = x*z; val xw = x*w; val ww = w*w
    val yy = y*y; val yz = y*z; val yw = y*w; val zz = z*z; val zw = z*w
    preMultiply(1-2*yy-2*zz, 2*(xy-zw),   2*(xz+yw),
                2*(xy+zw),   1-2*xx-2*zz, 2*(yz-xw),
                2*(xz-yw),   2*(yz+xw),   1-2*xx-2*yy)
  }

  @inline
  def preRotateQuat(q: Quat): Mat3 =
    preRotateQuat(q.x, q.y, q.z, q.w)

  @inline
  def postRotateQuat(x: Float, y: Float, z: Float, w: Float): Mat3 = {
    val xx = x*x; val xy = x*y; val xz = x*z; val xw = x*w; val ww = w*w
    val yy = y*y; val yz = y*z; val yw = y*w; val zz = z*z; val zw = z*w
    postMultiply(1-2*yy-2*zz, 2*(xy-zw),   2*(xz+yw),
                 2*(xy+zw),   1-2*xx-2*zz, 2*(yz-xw),
                 2*(xz-yw),   2*(yz+xw),   1-2*xx-2*yy)
  }

  @inline
  def postRotateQuat(q: Quat): Mat3 =
    postRotateQuat(q.x, q.y, q.z, q.w)

  @inline
  def preScale(x: Float, y: Float, z: Float): Mat3 =
    preMultiply(x, 0, 0,
                0, y, 0,
                0, 0, z)

  /**
   * Destructively pre-multiplies a rotation around the x-axis into this matrix.
   */
  @inline
  def preRotateX(angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    preMultiply(1, 0,  0,
                0, c, -s,
                0, s,  c)
  }

  /**
   * Destructively post-multiplies a rotation around the x-axis into this matrix.
   */
  @inline
  def postRotateX(angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    postMultiply(1, 0,  0,
                 0, c, -s,
                 0, s,  c)
  }

  /**
   * Destructively pre-multiplies a rotation around the y-axis into this matrix.
   */
  @inline
  def preRotateY(angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    preMultiply( c, 0, s,
                 0, 1, 0,
                -s, 0, c)
  }

  /**
   * Destructively post-multiplies a rotation around the y-axis into this matrix.
   */
  @inline
  def postRotateY(angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    postMultiply( c, 0, s,
                  0, 1, 0,
                 -s, 0, c)
  }

  /**
   * Destructively pre-multiplies a rotation around the z-axis into this matrix.
   */
  @inline
  def preRotateZ(angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    preMultiply( c, -s, 0,
                 s,  c, 0,
                 0,  0, 1)
  }

  /**
   * Destructively post-multiplies a rotation around the z-axis into this matrix.
   */
  @inline
  def postRotateZ(angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    postMultiply( c, -s, 0,
                  s,  c, 0,
                  0,  0, 1)
  }

  @inline
  def preScale(s: Vec3): Mat3 =
    preScale(s.x, s.y, s.z)

  @inline
  def postScale(x: Float, y: Float, z: Float): Mat3 =
    postMultiply(x, 0, 0,
                 0, y, 0,
                 0, 0, z)
  @inline
  def postScale(s: Vec3): Mat3 =
    postScale(s.x, s.y, s.z)

  @inline
  def determinant = {
    val b01 =  m22*m11 - m12*m21
    val b11 = -m22*m10 + m12*m20
    val b21 =  m21*m10 - m11*m20

    m00 * b01 + m01 * b11 + m02 * b21
  }

  @inline
  def inverted = {
    val b01 =  m22*m11 - m12*m21
    val b11 = -m22*m10 + m12*m20
    val b21 =  m21*m10 - m11*m20

    var det = m00 * b01 + m01 * b11 + m02 * b21
    assert(det != 0, "Matrix is not invertable")
    det = 1.0f / det

    Mat3(b01*det, (-m22*m01 + m02*m21) * det, ( m12*m01 - m02*m11)*det,
      b11*det, ( m22*m00 - m02*m20) * det, (-m12*m00 + m02*m10)*det,
      b21*det, (-m21*m00 + m01*m20) * det, ( m11*m00 - m01*m10)*det)
  }

  @inline
  def invert(out: Mat3 = this): Mat3 = {
    val b01 =  m22*m11 - m12*m21
    val b11 = -m22*m10 + m12*m20
    val b21 =  m21*m10 - m11*m20

    var det = m00 * b01 + m01 * b11 + m02 * b21
    assert(det != 0, "Matrix is not invertable")
    det = 1.0f / det

    out.set(b01*det, (-m22*m01 + m02*m21) * det, ( m12*m01 - m02*m11)*det,
            b11*det, ( m22*m00 - m02*m20) * det, (-m12*m00 + m02*m10)*det,
            b21*det, (-m21*m00 + m01*m20) * det, ( m11*m00 - m01*m10)*det)
  }

  @inline
  def adjointed =
    Mat3(m11*m22 - m12*m21, m02*m21 - m01*m22, m01*m12 - m02*m11,
         m12*m20 - m10*m22, m00*m22 - m02*m20, m02*m10 - m00*m12,
         m10*m21 - m11*m20, m01*m20 - m00*m21, m00*m11 - m01*m10)

  @inline
  def adjoint(out: Mat3 = this) =
    out.set(m11*m22 - m12*m21, m02*m21 - m01*m22, m01*m12 - m02*m11,
            m12*m20 - m10*m22, m00*m22 - m02*m20, m02*m10 - m00*m12,
            m10*m21 - m11*m20, m01*m20 - m00*m21, m00*m11 - m01*m10)

  @inline
  def toMat4 =
    Mat4(m00, m01, m02, 0,
      m10, m11, m12, 0,
      m20, m21, m22, 0,
      0,   0,   0,   1)

  @inline
  def toQuat = {
    val tr = m00 + m11 + m22

    if (tr > 0) {
      val S = math.sqrt(tr + 1.0f).toFloat * 2 // S=4*qw
      Quat((m21 - m12) / S, (m02 - m20) / S, (m10 - m01) / S, 0.25f * S).normalized
    } else if ((m00 > m11) & (m00 > m22)) {
      val S = math.sqrt(1.0 + m00 - m11 - m22).toFloat * 2 // S=4*qx
      Quat(0.25f * S, (m01 + m10) / S, (m02 + m20) / S,(m21 - m12) / S).normalized
    } else if (m11 > m22) {
      val S = math.sqrt(1.0 + m11 - m00 - m22).toFloat * 2 // S=4*qy
      Quat((m01 + m10) / S, 0.25f * S, (m12 + m21) / S, (m02 - m20) / S).normalized
    } else {
      val S = math.sqrt(1.0 + m22 - m00 - m11).toFloat * 2; // S=4*qz
      Quat((m02 + m20) / S, (m12 + m21) / S, 0.25f * S, (m10 - m01) / S).normalized
    }
  }

  @inline
  def toMat2d: Mat2d =
    Mat2d(m00, m01, m02,
          m10, m11, m12)


  @inline
  def copy(m00: Float = m00, m01: Float = m01, m02: Float = m02,
           m10: Float = m10, m11: Float = m11, m12: Float = m12,
           m20: Float = m20, m21: Float = m22, m22: Float = m22): Mat3 =
    Mat3(m00, m01, m02,
         m10, m11, m12,
         m20, m21, m22)

  override def toString =
    s"Mat3(${m00}f,${m01}f,${m02}f,${m10}f,${m11}f,${m12}f,${m20}f,${m21}f,${m22}f)"

  override def equals(o: Any): Boolean = o match {
    case m: Mat3 =>
      m00 == m.m00 && m01 == m.m01 && m02 == m.m02 &&
      m10 == m.m10 && m11 == m.m11 && m12 == m.m12 &&
      m20 == m.m20 && m21 == m.m21 && m22 == m.m22
    case _ => false
  }

  override def hashCode: Int =
    m00.hashCode()*19 + m01.hashCode()*23 + m02.hashCode()*29 +
    m10.hashCode()*31 + m11.hashCode()*37 + m12.hashCode()*41 +
    m20.hashCode()*43 + m21.hashCode()*47 + m22.hashCode()*53
}

object Mat3 {
  def apply(): Mat3 =
    new Mat3()

  def apply(m00: Float, m01: Float, m02: Float,
            m10: Float, m11: Float, m12: Float,
            m20: Float, m21: Float, m22: Float): Mat3 =
    new Mat3(m00, m01, m02,
             m10, m11, m12,
             m20, m21, m22)

  @inline
  def rotateQuat(q: Quat): Mat3 =
    rotateQuat(q.x, q.y, q.z, q.w)

  @inline
  def rotateQuat(x: Float, y: Float, z: Float, w: Float): Mat3 = {
    val xx = x*x; val xy = x*y; val xz = x*z; val xw = x*w; val ww = w*w;
    val yy = y*y; val yz = y*z; val yw = y*w; val zz = z*z; val zw = z*w;
    Mat3(1-2*yy-2*zz, 2*(xy-zw),   2*(xz+yw),
         2*(xy+zw),   1-2*xx-2*zz, 2*(yz-xw),
         2*(xz-yw),   2*(yz+xw),   1-2*xx-2*yy)
  }

  /**
   * Returns the rotation matrix about the given angle and axis.
   * @param angle the angle to rotate, in radians.
   * @param x the x-component of the axis vector to rotate around, must be normalized.
   * @param y the y-component of the axis vector to rotate around, must be normalized.
   * @param z the z-component of the axis vector to rotate around, must be normalized.
   */
  @inline
  def rotate(angle: Float, x: Float, y: Float, z: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    val t = 1-c

    Mat3(x*x*t + c,   x*y*t - z*s, x*z*t+y*s,
         y*x*t + z*s, y*y*t + c,   y*z*t-x*s,
         x*z*t - y*s, y*z*t + x*s, z*z*t + c)
  }

  /**
   * Returns the rotation matrix about the given angle and axis.
   * @param angle the angle to rotate, in radians.
   * @param axis the axis vector to rotate around, must be normalized.
   */
  @inline
  def rotate(angle: Float, axis: Vec3): Mat3 =
    rotate(angle, axis.x, axis.y, axis.z)

  /**
   * Returns the rotation matrix rotating around the X-axis.
   */
  @inline
  def rotateX(angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    Mat3(1, 0,  0,
         0, c, -s,
         0, s,  c)
  }

  /**
   * Returns the rotation matrix rotating around the Y-axis.
   */
  @inline
  def rotateY(angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    Mat3( c, 0, s,
          0, 1, 0,
         -s, 0, c)
  }

  /**
   * Returns the rotation matrix rotating around the Z-axis.
   */
  @inline
  def rotateZ(angle: Float): Mat3 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    Mat3( c, -s, 0,
          s,  c, 0,
          0,  0, 1)
  }

  /**
   * Returns a scale matrix
   * @param x the x-scale
   * @param y the y-scale
   * @param z the z-scale
   */
  @inline
  def scale(x: Float, y: Float, z: Float): Mat3 =
    Mat3(x, 0, 0,
         0, y, 0,
         0, 0, z)

  /**
   * Returns a scale matrix
   * @param s the scale vector
   */
  @inline
  def scale(s: Vec3): Mat3 =
    scale(s.x, s.y, s.z)

  @inline
  def scale2d(x: Float, y: Float): Mat3 =
    Mat3(x, 0, 0,
         0, y, 0,
         0, 0, 1)

  @inline
  def scale2d(v: Vec2): Mat3 =
    scale2d(v.x, v.y)

  @inline
  def rotate2d(angle: Float): Mat3 = {
    val c = math.cos(angle).toFloat
    val s = math.sin(angle).toFloat
    Mat3(c, -s, 0,
         s,  c, 0,
         0,  0, 1)
  }

  @inline
  def translate2d(x: Float, y: Float): Mat3 =
    Mat3(1, 0, x,
         0, 1, y,
         0, 0, 1)

  @inline
  def translate2d(t: Vec2): Mat3 =
    translate2d(t.x, t.y)
}
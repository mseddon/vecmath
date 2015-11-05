package scryetek.vecmath

/**
 * Represents a 4x4 matrix of Floats.
 */
@inline
final class Mat4(
   var m00: Float, var m01: Float, var m02: Float, var m03: Float,
   var m10: Float, var m11: Float, var m12: Float, var m13: Float,
   var m20: Float, var m21: Float, var m22: Float, var m23: Float,
   var m30: Float, var m31: Float, var m32: Float, var m33: Float
) {
  @inline
  def set(m00: Float = m00, m01: Float = m01, m02: Float = m02, m03: Float = m03,
          m10: Float = m10, m11: Float = m11, m12: Float = m12, m13: Float = m13,
          m20: Float = m20, m21: Float = m21, m22: Float = m22, m23: Float = m23,
          m30: Float = m30, m31: Float = m31, m32: Float = m33, m33: Float = m33): Mat4 = {
    this.m00 = m00; this.m01 = m01; this.m02 = m02; this.m03 = m03
    this.m10 = m10; this.m11 = m11; this.m12 = m12; this.m13 = m13
    this.m20 = m20; this.m21 = m21; this.m22 = m22; this.m23 = m23
    this.m30 = m30; this.m31 = m31; this.m32 = m32; this.m33 = m33
    this
  }

  @inline
  def set(m: Mat4): Mat4 =
    set(m.m00, m.m01, m.m02, m.m03,
        m.m10, m.m11, m.m12, m.m13,
        m.m20, m.m21, m.m22, m.m23,
        m.m30, m.m31, m.m32, m.m33)

  @inline
  def this() =
    this(1, 0, 0, 0,
         0, 1, 0, 0,
         0, 0, 1, 0,
         0, 0, 0, 1)

  @inline
  def transposed =
    Mat4(m00, m10, m20, m30,
         m01, m11, m21, m31,
         m02, m12, m22, m32,
         m03, m13, m23, m33)

  @inline
  def transpose(out: Mat4 = this): Mat4 =
    out.set(m00, m10, m20, m30,
            m01, m11, m21, m31,
            m02, m12, m22, m32,
            m03, m13, m23, m33)

  @inline
  def *(v: Vec4): Vec4 =
    Vec4(m00 * v.x + m01 * v.y + m02 * v.z + m03 * v.w,
         m10 * v.x + m11 * v.y + m12 * v.z + m13 * v.w,
         m20 * v.x + m21 * v.y + m22 * v.z + m23 * v.w,
         m30 * v.x + m31 * v.y + m32 * v.z + m33 * v.w)

  @inline
  def *(q: Quat): Mat4 = {
    val xx = q.x*q.x; val xy = q.x*q.y; val xz = q.x*q.z; val xw = q.x*q.w; val ww = q.w*q.w
    val yy = q.y*q.y; val yz = q.y*q.z; val yw = q.y*q.w; val zz = q.z*q.z; val zw = q.z*q.w

    val b00 = 1-2*yy-2*zz; val b01 = 2*(xy-zw);   val b02 = 2*(xz+yw);   val b03 = 0
    val b10 = 2*(xy+zw);   val b11 = 1-2*xx-2*zz; val b12 = 2*(yz-xw);   val b13 = 0
    val b20 = 2*(xz-yw);   val b21 = 2*(yz+xw);   val b22 = 1-2*xx-2*yy; val b23 = 0
    val b30 = 0;           val b31 = 0;           val b32 = 0;           val b33 = 1
    Mat4(b00 * m00 + b10 * m01 + b20 * m02 + b30 * m03,
         b01 * m00 + b11 * m01 + b21 * m02 + b31 * m03,
         b02 * m00 + b12 * m01 + b22 * m02 + b32 * m03,
         b03 * m00 + b13 * m01 + b23 * m02 + b33 * m03,

         b00 * m10 + b10 * m11 + b20 * m12 + b30 * m13,
         b01 * m10 + b11 * m11 + b21 * m12 + b31 * m13,
         b02 * m10 + b12 * m11 + b22 * m12 + b32 * m13,
         b03 * m10 + b13 * m11 + b23 * m12 + b33 * m13,

         b00 * m20 + b10 * m21 + b20 * m22 + b30 * m23,
         b01 * m20 + b11 * m21 + b21 * m22 + b31 * m23,
         b02 * m20 + b12 * m21 + b22 * m22 + b32 * m23,
         b03 * m20 + b13 * m21 + b23 * m22 + b33 * m23,

         b00 * m30 + b10 * m31 + b20 * m32 + b30 * m33,
         b01 * m30 + b11 * m31 + b21 * m32 + b31 * m33,
         b02 * m30 + b12 * m31 + b22 * m32 + b32 * m33,
         b03 * m30 + b13 * m31 + b23 * m32 + b33 * m33)
  }

  @inline
  def mul(v: Vec4, out: Vec4): Vec4 =
    out.set(m00 * v.x + m01 * v.y + m02 * v.z + m03 * v.w,
            m10 * v.x + m11 * v.y + m12 * v.z + m13 * v.w,
            m20 * v.x + m21 * v.y + m22 * v.z + m23 * v.w,
            m30 * v.x + m31 * v.y + m32 * v.z + m33 * v.w)

  @inline
  def *(s: Float): Mat4 =
    Mat4(m00*s, m01*s, m02*s, m03*s,
         m10*s, m11*s, m12*s, m13*s,
         m20*s, m21*s, m22*s, m23*s,
         m30*s, m31*s, m32*s, m33*s)

  @inline
  def scale(s: Float, out: Mat4 = this): Mat4 =
    out.set(m00*s, m01*s, m02*s, m03*s,
            m10*s, m11*s, m12*s, m13*s,
            m20*s, m21*s, m22*s, m23*s,
            m30*s, m31*s, m32*s, m33*s)

  @inline
  def *(b: Mat4): Mat4 =
    Mat4(b.m00 * m00 + b.m10 * m01 + b.m20 * m02 + b.m30 * m03,
         b.m01 * m00 + b.m11 * m01 + b.m21 * m02 + b.m31 * m03,
         b.m02 * m00 + b.m12 * m01 + b.m22 * m02 + b.m32 * m03,
         b.m03 * m00 + b.m13 * m01 + b.m23 * m02 + b.m33 * m03,

         b.m00 * m10 + b.m10 * m11 + b.m20 * m12 + b.m30 * m13,
         b.m01 * m10 + b.m11 * m11 + b.m21 * m12 + b.m31 * m13,
         b.m02 * m10 + b.m12 * m11 + b.m22 * m12 + b.m32 * m13,
         b.m03 * m10 + b.m13 * m11 + b.m23 * m12 + b.m33 * m13,

         b.m00 * m20 + b.m10 * m21 + b.m20 * m22 + b.m30 * m23,
         b.m01 * m20 + b.m11 * m21 + b.m21 * m22 + b.m31 * m23,
         b.m02 * m20 + b.m12 * m21 + b.m22 * m22 + b.m32 * m23,
         b.m03 * m20 + b.m13 * m21 + b.m23 * m22 + b.m33 * m23,

         b.m00 * m30 + b.m10 * m31 + b.m20 * m32 + b.m30 * m33,
         b.m01 * m30 + b.m11 * m31 + b.m21 * m32 + b.m31 * m33,
         b.m02 * m30 + b.m12 * m31 + b.m22 * m32 + b.m32 * m33,
         b.m03 * m30 + b.m13 * m31 + b.m23 * m32 + b.m33 * m33)

  @inline
  def postMultiply(b: Mat4, out: Mat4): Mat4 =
    postMultiply(b.m00, b.m01, b.m02, b.m03,
                 b.m10, b.m11, b.m12, b.m13,
                 b.m20, b.m21, b.m22, b.m23,
                 b.m30, b.m31, b.m32, b.m33,
                 out)

  @inline
  def postMultiply(b: Mat4): Mat4 =
    postMultiply(b.m00, b.m01, b.m02, b.m03,
                b.m10, b.m11, b.m12, b.m13,
                b.m20, b.m21, b.m22, b.m23,
                b.m30, b.m31, b.m32, b.m33)

  @inline
  def postMultiply(b00: Float, b01: Float, b02: Float, b03: Float,
                   b10: Float, b11: Float, b12: Float, b13: Float,
                   b20: Float, b21: Float, b22: Float, b23: Float,
                   b30: Float, b31: Float, b32: Float, b33: Float,
                   out: Mat4 = this): Mat4 =
    out.set(
      b00 * m00 + b10 * m01 + b20 * m02 + b30 * m03,
      b01 * m00 + b11 * m01 + b21 * m02 + b31 * m03,
      b02 * m00 + b12 * m01 + b22 * m02 + b32 * m03,
      b03 * m00 + b13 * m01 + b23 * m02 + b33 * m03,

      b00 * m10 + b10 * m11 + b20 * m12 + b30 * m13,
      b01 * m10 + b11 * m11 + b21 * m12 + b31 * m13,
      b02 * m10 + b12 * m11 + b22 * m12 + b32 * m13,
      b03 * m10 + b13 * m11 + b23 * m12 + b33 * m13,

      b00 * m20 + b10 * m21 + b20 * m22 + b30 * m23,
      b01 * m20 + b11 * m21 + b21 * m22 + b31 * m23,
      b02 * m20 + b12 * m21 + b22 * m22 + b32 * m23,
      b03 * m20 + b13 * m21 + b23 * m22 + b33 * m23,

      b00 * m30 + b10 * m31 + b20 * m32 + b30 * m33,
      b01 * m30 + b11 * m31 + b21 * m32 + b31 * m33,
      b02 * m30 + b12 * m31 + b22 * m32 + b32 * m33,
      b03 * m30 + b13 * m31 + b23 * m32 + b33 * m33
    )

  @inline
  def preMultiply(b: Mat4): Mat4 =
    preMultiply(b.m00, b.m01, b.m02, b.m03,
                b.m10, b.m11, b.m12, b.m13,
                b.m20, b.m21, b.m22, b.m23,
                b.m30, b.m31, b.m32, b.m33)

  @inline
  def preMultiply(b: Mat4, out: Mat4): Mat4 =
    preMultiply(b.m00, b.m01, b.m02, b.m03,
                b.m10, b.m11, b.m12, b.m13,
                b.m20, b.m21, b.m22, b.m23,
                b.m30, b.m31, b.m32, b.m33,
                out)

  /**
   * Premultiplies the given matrix with this matrix, into the output matrix, non allocation version.
   * allocations.
   */
  @inline
  def preMultiply(
     b00: Float, b01: Float, b02: Float, b03: Float,
     b10: Float, b11: Float, b12: Float, b13: Float,
     b20: Float, b21: Float, b22: Float, b23: Float,
     b30: Float, b31: Float, b32: Float, b33: Float,
     out: Mat4 = this): Mat4 =
    out.set(m00 * b00 + m10 * b01 + m20 * b02 + m30 * b03,
            m01 * b00 + m11 * b01 + m21 * b02 + m31 * b03,
            m02 * b00 + m12 * b01 + m22 * b02 + m32 * b03,
            m03 * b00 + m13 * b01 + m23 * b02 + m33 * b03,

            m00 * b10 + m10 * b11 + m20 * b12 + m30 * b13,
            m01 * b10 + m11 * b11 + m21 * b12 + m31 * b13,
            m02 * b10 + m12 * b11 + m22 * b12 + m32 * b13,
            m03 * b10 + m13 * b11 + m23 * b12 + m33 * b13,

            m00 * b20 + m10 * b21 + m20 * b22 + m30 * b23,
            m01 * b20 + m11 * b21 + m21 * b22 + m31 * b23,
            m02 * b20 + m12 * b21 + m22 * b22 + m32 * b23,
            m03 * b20 + m13 * b21 + m23 * b22 + m33 * b23,

            m00 * b30 + m10 * b31 + m20 * b32 + m30 * b33,
            m01 * b30 + m11 * b31 + m21 * b32 + m31 * b33,
            m02 * b30 + m12 * b31 + m22 * b32 + m32 * b33,
            m03 * b30 + m13 * b31 + m23 * b32 + m33 * b33)

  /**
   * Destructively pre-multiplies a rotation around the axis into this matrix.
   * @note the axis must be normalized.
   */
  @inline
  def preRotate(angle: Float, x: Float, y: Float, z: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    val t = 1-c

    preMultiply(x*x*t + c,   x*y*t - z*s, x*z*t+y*s, 0,
               y*x*t + z*s, y*y*t + c,   y*z*t-x*s, 0,
               x*z*t - y*s, y*z*t + x*s, z*z*t + c, 0,
               0,           0,           0,         1)
  }

  /**
   * Destructively pre-multiplies a rotation around the axis into this matrix.
   * @note the axis must be normalized.
   */
  @inline
  def preRotate(angle: Float, axis: Vec3): Mat4 =
    preRotate(angle, axis.x, axis.y, axis.z)

  /**
   * Destructively post-multiplies a rotation around the axis into this matrix.
   * @note the axis must be normalized.
   */
  @inline
  def postRotate(angle: Float, x: Float, y: Float, z: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    val t = 1-c

    postMultiply(x*x*t + c,   x*y*t - z*s, x*z*t+y*s, 0,
                 y*x*t + z*s, y*y*t + c,   y*z*t-x*s, 0,
                 x*z*t - y*s, y*z*t + x*s, z*z*t + c, 0,
                 0,           0,           0,         1)
  }

  /**
   * Destructively post-multiplies a rotation around the axis into this matrix.
   * @note the axis must be normalized.
   */
  @inline
  def postRotate(angle: Float, axis: Vec3): Mat4 =
    postRotate(angle, axis.x, axis.y, axis.z)

  @inline
  def preRotateQuat(x: Float, y: Float, z: Float, w: Float): Mat4 = {
    val xx = x*x; val xy = x*y; val xz = x*z; val xw = x*w; val ww = w*w;
    val yy = y*y; val yz = y*z; val yw = y*w; val zz = z*z; val zw = z*w;
    preMultiply(1-2*yy-2*zz, 2*(xy-zw),   2*(xz+yw),   0,
                2*(xy+zw),   1-2*xx-2*zz, 2*(yz-xw),   0,
                2*(xz-yw),   2*(yz+xw),   1-2*xx-2*yy, 0,
                0,           0,           0,           1)
  }

  @inline
  def preRotateQuat(q: Quat): Mat4 =
    preRotateQuat(q.x, q.y, q.z, q.w)

  @inline
  def postRotateQuat(x: Float, y: Float, z: Float, w: Float): Mat4 = {
    val xx = x*x; val xy = x*y; val xz = x*z; val xw = x*w; val ww = w*w;
    val yy = y*y; val yz = y*z; val yw = y*w; val zz = z*z; val zw = z*w;
    postMultiply(1-2*yy-2*zz, 2*(xy-zw),   2*(xz+yw),   0,
                 2*(xy+zw),   1-2*xx-2*zz, 2*(yz-xw),   0,
                 2*(xz-yw),   2*(yz+xw),   1-2*xx-2*yy, 0,
                 0,           0,           0,           1)
  }

  @inline
  def postRotateQuat(q: Quat): Mat4 =
    postRotateQuat(q.x, q.y, q.z, q.w)

  /**
   * Destructively pre-multiplies a rotation around the x-axis into this matrix.
   */
  @inline
  def preRotateX(angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    preMultiply(1, 0,  0, 0,
                0, c, -s, 0,
                0, s,  c, 0,
                0, 0,  0, 1)
  }

  /**
   * Destructively post-multiplies a rotation around the x-axis into this matrix.
   */
  @inline
  def postRotateX(angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    postMultiply(1, 0,  0, 0,
                 0, c, -s, 0,
                 0, s,  c, 0,
                 0, 0,  0, 1)
  }

  /**
   * Destructively pre-multiplies a rotation around the y-axis into this matrix.
   */
  @inline
  def preRotateY(angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    preMultiply( c, 0, s, 0,
                 0, 1, 0, 0,
                -s, 0, c, 0,
                 0, 0, 0, 1)
  }

  /**
   * Destructively post-multiplies a rotation around the y-axis into this matrix.
   */
  @inline
  def postRotateY(angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    postMultiply( c, 0, s, 0,
                  0, 1, 0, 0,
                 -s, 0, c, 0,
                  0, 0, 0, 1)
  }

  /**
   * Destructively pre-multiplies a rotation around the z-axis into this matrix.
   */
  @inline
  def preRotateZ(angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    preMultiply( c, -s, 0, 0,
                 s,  c, 0, 0,
                 0,  0, 1, 0,
                 0,  0, 0, 1)
  }

  /**
   * Destructively post-multiplies a rotation around the z-axis into this matrix.
   */
  @inline
  def postRotateZ(angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    postMultiply( c, -s, 0, 0,
                  s,  c, 0, 0,
                  0,  0, 1, 0,
                  0,  0, 0, 1)
  }

  @inline
  def preScale(x: Float, y: Float, z: Float): Mat4 =
    preMultiply(x, 0, 0, 0,
                0, y, 0, 0,
                0, 0, z, 0,
                0, 0, 0, 1)
  @inline
  def preScale(s: Vec3): Mat4 =
    preScale(s.x, s.y, s.z)

  @inline
  def postScale(x: Float, y: Float, z: Float): Mat4 =
    postMultiply(x, 0, 0, 0,
                 0, y, 0, 0,
                 0, 0, z, 0,
                 0, 0, 0, 1)
  @inline
  def postScale(s: Vec3): Mat4 =
    postScale(s.x, s.y, s.z)

  @inline
  def preTranslate(x: Float, y: Float, z: Float): Mat4 =
    preMultiply(1, 0, 0, x,
                0, 1, 0, y,
                0, 0, 1, z,
                0, 0, 0, 1)
  @inline
  def preTranslate(t: Vec3): Mat4 =
    preTranslate(t.x, t.y, t.z)

  @inline
  def postTranslate(x: Float, y: Float, z: Float): Mat4 =
    postMultiply(1, 0, 0, x,
                 0, 1, 0, y,
                 0, 0, 1, z,
                 0, 0, 0, 1)

  @inline
  def postTranslate(t: Vec3): Mat4 =
    postTranslate(t.x, t.y, t.z)

  @inline
  def +(b: Mat4): Mat4 =
    Mat4(m00 + b.m00, m01 + b.m01, m02 + b.m02, m03 + b.m03,
         m10 + b.m10, m11 + b.m11, m12 + b.m12, m13 + b.m13,
         m20 + b.m20, m21 + b.m21, m22 + b.m22, m23 + b.m23,
         m30 + b.m30, m31 + b.m31, m32 + b.m32, m33 + b.m33)

  @inline
  def add(b: Mat4, out: Mat4 = this): Mat4 =
    out.set(m00 + b.m00, m01 + b.m01, m02 + b.m02, m03 + b.m03,
            m10 + b.m10, m11 + b.m11, m12 + b.m12, m13 + b.m13,
            m20 + b.m20, m21 + b.m21, m22 + b.m22, m23 + b.m23,
            m30 + b.m30, m31 + b.m31, m32 + b.m32, m33 + b.m33)


  @inline
  def -(b: Mat4): Mat4 =
    Mat4(m00 - b.m00, m01 - b.m01, m02 - b.m02, m03 - b.m03,
         m10 - b.m10, m11 - b.m11, m12 - b.m12, m13 - b.m13,
         m20 - b.m20, m21 - b.m21, m22 - b.m22, m23 - b.m23,
         m30 - b.m30, m31 - b.m31, m32 - b.m32, m33 - b.m33)

  @inline
  def sub(b: Mat4, out: Mat4 = this): Mat4 =
    out.set(m00 - b.m00, m01 - b.m01, m02 - b.m02, m03 - b.m03,
            m10 - b.m10, m11 - b.m11, m12 - b.m12, m13 - b.m13,
            m20 - b.m20, m21 - b.m21, m22 - b.m22, m23 - b.m23,
            m30 - b.m30, m31 - b.m31, m32 - b.m32, m33 - b.m33)

  @inline
  def determinant = {
    val b00 = m00 * m11 - m01 * m10
    val b01 = m00 * m12 - m02 * m10
    val b02 = m00 * m13 - m03 * m10
    val b03 = m01 * m12 - m02 * m11
    val b04 = m01 * m13 - m03 * m11
    val b05 = m02 * m13 - m03 * m12
    val b06 = m20 * m31 - m21 * m30
    val b07 = m20 * m32 - m22 * m30
    val b08 = m20 * m33 - m23 * m30
    val b09 = m21 * m32 - m22 * m31
    val b10 = m21 * m33 - m23 * m31
    val b11 = m22 * m33 - m23 * m32

    b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06
  }

  @inline
  def inverted = {
    val b00 = m00 * m11 - m01 * m10
    val b01 = m00 * m12 - m02 * m10
    val b02 = m00 * m13 - m03 * m10
    val b03 = m01 * m12 - m02 * m11
    val b04 = m01 * m13 - m03 * m11
    val b05 = m02 * m13 - m03 * m12
    val b06 = m20 * m31 - m21 * m30
    val b07 = m20 * m32 - m22 * m30
    val b08 = m20 * m33 - m23 * m30
    val b09 = m21 * m32 - m22 * m31
    val b10 = m21 * m33 - m23 * m31
    val b11 = m22 * m33 - m23 * m32

    var det = b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06
    assert(det != 0, "Matrix is not invertable")
    det = 1.0f / det

    Mat4((m11 * b11 - m12 * b10 + m13 * b09) * det,
         (m02 * b10 - m01 * b11 - m03 * b09) * det,
         (m31 * b05 - m32 * b04 + m33 * b03) * det,
         (m22 * b04 - m21 * b05 - m23 * b03) * det,

         (m12 * b08 - m10 * b11 - m13 * b07) * det,
         (m00 * b11 - m02 * b08 + m03 * b07) * det,
         (m32 * b02 - m30 * b05 - m33 * b01) * det,
         (m20 * b05 - m22 * b02 + m23 * b01) * det,

         (m10 * b10 - m11 * b08 + m13 * b06) * det,
         (m01 * b08 - m00 * b10 - m03 * b06) * det,
         (m30 * b04 - m31 * b02 + m33 * b00) * det,
         (m21 * b02 - m20 * b04 - m23 * b00) * det,

         (m11 * b07 - m10 * b09 - m12 * b06) * det,
         (m00 * b09 - m01 * b07 + m02 * b06) * det,
         (m31 * b01 - m30 * b03 - m32 * b00) * det,
         (m20 * b03 - m21 * b01 + m22 * b00) * det)
  }

  def invert(out: Mat4 = this): Mat4 = {
    val b00 = m00 * m11 - m01 * m10
    val b01 = m00 * m12 - m02 * m10
    val b02 = m00 * m13 - m03 * m10
    val b03 = m01 * m12 - m02 * m11
    val b04 = m01 * m13 - m03 * m11
    val b05 = m02 * m13 - m03 * m12
    val b06 = m20 * m31 - m21 * m30
    val b07 = m20 * m32 - m22 * m30
    val b08 = m20 * m33 - m23 * m30
    val b09 = m21 * m32 - m22 * m31
    val b10 = m21 * m33 - m23 * m31
    val b11 = m22 * m33 - m23 * m32

    var det = b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06
    assert(det != 0, "Matrix is not invertable")
    det = 1.0f / det

    out.set((m11 * b11 - m12 * b10 + m13 * b09) * det,
            (m02 * b10 - m01 * b11 - m03 * b09) * det,
            (m31 * b05 - m32 * b04 + m33 * b03) * det,
            (m22 * b04 - m21 * b05 - m23 * b03) * det,

            (m12 * b08 - m10 * b11 - m13 * b07) * det,
            (m00 * b11 - m02 * b08 + m03 * b07) * det,
            (m32 * b02 - m30 * b05 - m33 * b01) * det,
            (m20 * b05 - m22 * b02 + m23 * b01) * det,

            (m10 * b10 - m11 * b08 + m13 * b06) * det,
            (m01 * b08 - m00 * b10 - m03 * b06) * det,
            (m30 * b04 - m31 * b02 + m33 * b00) * det,
            (m21 * b02 - m20 * b04 - m23 * b00) * det,

            (m11 * b07 - m10 * b09 - m12 * b06) * det,
            (m00 * b09 - m01 * b07 + m02 * b06) * det,
            (m31 * b01 - m30 * b03 - m32 * b00) * det,
            (m20 * b03 - m21 * b01 + m22 * b00) * det)
  }

  /** Returns the adjoint of the matrix. */
  @inline
  def adjointed =
    Mat4(
      m11 * (m22 * m33 - m23 * m32) - m21 * (m12 * m33 - m13 * m32) + m31 * (m12 * m23 - m13 * m22),
      -(m01 * (m22 * m33 - m23 * m32) - m21 * (m02 * m33 - m03 * m32) + m31 * (m02 * m23 - m03 * m22)),
      m01 * (m12 * m33 - m13 * m32) - m11 * (m02 * m33 - m03 * m32) + m31 * (m02 * m13 - m03 * m12),
      -(m01 * (m12 * m23 - m13 * m22) - m11 * (m02 * m23 - m03 * m22) + m21 * (m02 * m13 - m03 * m12)),
      -(m10 * (m22 * m33 - m23 * m32) - m20 * (m12 * m33 - m13 * m32) + m30 * (m12 * m23 - m13 * m22)),
      m00 * (m22 * m33 - m23 * m32) - m20 * (m02 * m33 - m03 * m32) + m30 * (m02 * m23 - m03 * m22),
      -(m00 * (m12 * m33 - m13 * m32) - m10 * (m02 * m33 - m03 * m32) + m30 * (m02 * m13 - m03 * m12)),
      m00 * (m12 * m23 - m13 * m22) - m10 * (m02 * m23 - m03 * m22) + m20 * (m02 * m13 - m03 * m12),
      m10 * (m21 * m33 - m23 * m31) - m20 * (m11 * m33 - m13 * m31) + m30 * (m11 * m23 - m13 * m21),
      -(m00 * (m21 * m33 - m23 * m31) - m20 * (m01 * m33 - m03 * m31) + m30 * (m01 * m23 - m03 * m21)),
      m00 * (m11 * m33 - m13 * m31) - m10 * (m01 * m33 - m03 * m31) + m30 * (m01 * m13 - m03 * m11),
      -(m00 * (m11 * m23 - m13 * m21) - m10 * (m01 * m23 - m03 * m21) + m20 * (m01 * m13 - m03 * m11)),
      -(m10 * (m21 * m32 - m22 * m31) - m20 * (m11 * m32 - m12 * m31) + m30 * (m11 * m22 - m12 * m21)),
      m00 * (m21 * m32 - m22 * m31) - m20 * (m01 * m32 - m02 * m31) + m30 * (m01 * m22 - m02 * m21),
      -(m00 * (m11 * m32 - m12 * m31) - m10 * (m01 * m32 - m02 * m31) + m30 * (m01 * m12 - m02 * m11)),
      m00 * (m11 * m22 - m12 * m21) - m10 * (m01 * m22 - m02 * m21) + m20 * (m01 * m12 - m02 * m11))

  /** Returns the adjoint of the matrix. */
  @inline
  def adjointed(out: Mat4 = this): Mat4 =
    out.set(
      m11 * (m22 * m33 - m23 * m32) - m21 * (m12 * m33 - m13 * m32) + m31 * (m12 * m23 - m13 * m22),
      -(m01 * (m22 * m33 - m23 * m32) - m21 * (m02 * m33 - m03 * m32) + m31 * (m02 * m23 - m03 * m22)),
      m01 * (m12 * m33 - m13 * m32) - m11 * (m02 * m33 - m03 * m32) + m31 * (m02 * m13 - m03 * m12),
      -(m01 * (m12 * m23 - m13 * m22) - m11 * (m02 * m23 - m03 * m22) + m21 * (m02 * m13 - m03 * m12)),
      -(m10 * (m22 * m33 - m23 * m32) - m20 * (m12 * m33 - m13 * m32) + m30 * (m12 * m23 - m13 * m22)),
      m00 * (m22 * m33 - m23 * m32) - m20 * (m02 * m33 - m03 * m32) + m30 * (m02 * m23 - m03 * m22),
      -(m00 * (m12 * m33 - m13 * m32) - m10 * (m02 * m33 - m03 * m32) + m30 * (m02 * m13 - m03 * m12)),
      m00 * (m12 * m23 - m13 * m22) - m10 * (m02 * m23 - m03 * m22) + m20 * (m02 * m13 - m03 * m12),
      m10 * (m21 * m33 - m23 * m31) - m20 * (m11 * m33 - m13 * m31) + m30 * (m11 * m23 - m13 * m21),
      -(m00 * (m21 * m33 - m23 * m31) - m20 * (m01 * m33 - m03 * m31) + m30 * (m01 * m23 - m03 * m21)),
      m00 * (m11 * m33 - m13 * m31) - m10 * (m01 * m33 - m03 * m31) + m30 * (m01 * m13 - m03 * m11),
      -(m00 * (m11 * m23 - m13 * m21) - m10 * (m01 * m23 - m03 * m21) + m20 * (m01 * m13 - m03 * m11)),
      -(m10 * (m21 * m32 - m22 * m31) - m20 * (m11 * m32 - m12 * m31) + m30 * (m11 * m22 - m12 * m21)),
      m00 * (m21 * m32 - m22 * m31) - m20 * (m01 * m32 - m02 * m31) + m30 * (m01 * m22 - m02 * m21),
      -(m00 * (m11 * m32 - m12 * m31) - m10 * (m01 * m32 - m02 * m31) + m30 * (m01 * m12 - m02 * m11)),
      m00 * (m11 * m22 - m12 * m21) - m10 * (m01 * m22 - m02 * m21) + m20 * (m01 * m12 - m02 * m11))

  @inline
  def toAngleAxis: AngleAxis = {
    // Shamelessly cribbed and ported from the java version at
    // http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToAngle/

    val epsilon = 0.01f; // margin to allow for rounding errors
    val epsilon2 = 0.1f; // margin to distinguish between 0 and 180 degrees
    if (math.abs(m01 - m10) < epsilon && math.abs(m02-m20) < epsilon && math.abs(m12-m21) < epsilon) {
      // singularity found
      // first check for identity matrix which must have +1 for all terms
      //  in leading diagonal and zero in other terms
      if (math.abs(m01 + m10) < epsilon2 && math.abs(m02+m20) < epsilon2 &&
          math.abs(m12+m21) < epsilon2 && math.abs(m00 + m11 + m22 - 3) < epsilon2) {
        // this singularity is identity matrix so angle = 0
        return AngleAxis(0,Vec3(1,0,0)); // zero angle, arbitrary axis
      }
      // otherwise this singularity is angle = 180
      val angle = Math.PI.toFloat
      val xx = (m00 + 1) / 2
      val yy = (m11 + 1) / 2
      val zz = (m22 + 1) / 2
      val xy = (m01 + m10) / 4
      val xz = (m02 + m20) / 4
      val yz = (m12 + m21) / 4
      if (xx > yy && xx > zz) {
        // m00 is the largest diagonal term
        if (xx < epsilon)
          return AngleAxis(angle, Vec3(0, 0.7071f, 0.7071f))
        else {
          val x = Math.sqrt(xx).toFloat
          return AngleAxis(angle, Vec3(x, xy / x, xz / x))
        }
      } else if (yy > zz) {
        // m11 is the largest diagonal term
        if (yy < epsilon)
          return AngleAxis(angle, Vec3(0.7071f, 0, 0.7071f))
        else {
          val y = Math.sqrt(yy).toFloat
          return AngleAxis(angle, Vec3(xy / y, y, yz / y))
        }
      } else {
        // m22 is the largest diagonal term so base result on this
        if (zz < epsilon) {
          return AngleAxis(angle, Vec3(0.7071f, 0.7071f, 0))
        } else {
          val z = Math.sqrt(zz).toFloat
          return AngleAxis(angle, Vec3(xz / z, yz / z, z))
        }
      }
    }
    // as we have reached here there are no singularities so we can handle normally
    var s = math.sqrt((m21 - m12)*(m21 - m12) + (m02 - m20)*(m02 - m20) + (m10 - m01)*(m10 - m01)).toFloat // used to normalise
    if (Math.abs(s) < 0.001) s=1
    // prevent divide by zero, should not happen if matrix is orthogonal and should be
    // caught by singularity test above, but I've left it in just in case
    AngleAxis(Math.acos((m00 + m11 + m22 - 1)/2).toFloat, Vec3((m21 - m12)/s, (m02 - m20)/s, (m10 - m01)/s))
  }

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

  def copy(m00: Float = m00, m01: Float = m01, m02: Float = m02, m03: Float = m03,
           m10: Float = m10, m11: Float = m11, m12: Float = m12, m13: Float = m13,
           m20: Float = m20, m21: Float = m21, m22: Float = m22, m23: Float = m23,
           m30: Float = m30, m31: Float = m31, m32: Float = m33, m33: Float = m33): Mat4 =
    set(m00, m01, m02, m03,
        m10, m11, m12, m13,
        m20, m21, m22, m23,
        m30, m31, m32, m33)

  override def toString =
    s"Mat4(${m00}f,${m01}f,${m02}f,${m03}f, ${m10}f,${m11}f,${m12}f,${m13}f,${m20}f,${m21}f,${m22}f,${m23}f)"

  override def equals(o: Any): Boolean = o match {
    case m: Mat4 =>
      m00 == m.m00 && m01 == m.m01 && m02 == m.m02 && m03 == m.m03 &&
      m10 == m.m10 && m11 == m.m11 && m12 == m.m12 && m13 == m.m13 &&
      m20 == m.m20 && m21 == m.m21 && m22 == m.m22 && m23 == m.m23 &&
      m30 == m.m30 && m31 == m.m31 && m32 == m.m32 && m33 == m.m33
    case _ => false
  }

  override def hashCode: Int =
    m00.hashCode()*19 + m01.hashCode()*23 + m02.hashCode()*29 + m03.hashCode()*31 +
    m10.hashCode()*37 + m11.hashCode()*41 + m12.hashCode()*43 + m13.hashCode()*47 +
    m20.hashCode()*53 + m21.hashCode()*59 + m22.hashCode()*61 + m22.hashCode()*67 +
    m30.hashCode()*71 + m31.hashCode()*73 + m32.hashCode()*79 + m32.hashCode()*83
}

object Mat4 {
  def apply(m00: Float, m01: Float, m02: Float, m03: Float,
            m10: Float, m11: Float, m12: Float, m13: Float,
            m20: Float, m21: Float, m22: Float, m23: Float,
            m30: Float, m31: Float, m32: Float, m33: Float): Mat4 =
    new Mat4(m00, m01, m02, m03,
             m10, m11, m12, m13,
             m20, m21, m22, m23,
             m30, m31, m32, m33)

  def apply(): Mat4 =
    new Mat4()

  @inline
  def rotateQuat(q: Quat): Mat4 =
    rotateQuat(q.x, q.y, q.z, q.w)

  @inline
  def rotateQuat(x: Float, y: Float, z: Float, w: Float): Mat4 = {
    val xx = x*x; val xy = x*y; val xz = x*z; val xw = x*w; val ww = w*w;
    val yy = y*y; val yz = y*z; val yw = y*w; val zz = z*z; val zw = z*w;
    Mat4(
      1-2*yy-2*zz, 2*(xy-zw),   2*(xz+yw),   0,
      2*(xy+zw),   1-2*xx-2*zz, 2*(yz-xw),   0,
      2*(xz-yw),   2*(yz+xw),   1-2*xx-2*yy, 0,
      0,           0,           0,           1)
  }

  /**
   * Returns the rotation matrix about the given angle and axis.
   * @param angle the angle to rotate, in radians.
   * @param x the x-component of the axis vector to rotate around, must be normalized.
   * @param y the y-component of the axis vector to rotate around, must be normalized.
   * @param z the z-component of the axis vector to rotate around, must be normalized.
   */
  @inline
  def rotate(angle: Float, x: Float, y: Float, z: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    val t = 1-c

    Mat4(x*x*t + c,   x*y*t - z*s, x*z*t+y*s, 0,
         y*x*t + z*s, y*y*t + c,   y*z*t-x*s, 0,
         x*z*t - y*s, y*z*t + x*s, z*z*t + c, 0,
         0,           0,           0,         1)
  }

  /**
   * Returns the rotation matrix about the given angle and axis.
   * @param angle the angle to rotate, in radians.
   * @param axis the axis vector to rotate around, must be normalized.
   */
  @inline
  def rotate(angle: Float, axis: Vec3): Mat4 =
    rotate(angle, axis.x, axis.y, axis.z)

  /**
   * Returns the rotation matrix rotating around the X-axis.
   */
  @inline
  def rotateX(angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    Mat4(1, 0,  0, 0,
         0, c, -s, 0,
         0, s,  c, 0,
         0, 0,  0, 1)
  }

  /**
   * Returns the rotation matrix rotating around the Y-axis.
   */
  @inline
  def rotateY(angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    Mat4( c, 0, s, 0,
          0, 1, 0, 0,
         -s, 0, c, 0,
          0, 0, 0, 1)
  }

  /**
   * Returns the rotation matrix rotating around the Z-axis.
   */
  @inline
  def rotateZ(angle: Float): Mat4 = {
    val s = Math.sin(angle).toFloat
    val c = Math.cos(angle).toFloat
    Mat4( c, -s, 0, 0,
          s,  c, 0, 0,
          0,  0, 1, 0,
          0,  0, 0, 1)
  }

  /**
   * Returns a scale matrix
   * @param x the x-scale
   * @param y the y-scale
   * @param z the z-scale
   */
  @inline
  def scale(x: Float, y: Float, z: Float): Mat4 =
    Mat4(x, 0, 0, 0,
         0, y, 0, 0,
         0, 0, z, 0,
         0, 0, 0, 1)

  /**
   * Returns a scale matrix
   * @param s the scale vector
   */
  @inline
  def scale(s: Vec3): Mat4 =
    scale(s.x, s.y, s.z)

  /**
   * Returns a translate matrix
   * @param x the x-translation
   * @param y the y-translation
   * @param z the z-translation
   */
  @inline
  def translate(x: Float, y: Float, z: Float): Mat4 =
    Mat4(1, 0, 0, x,
         0, 1, 0, y,
         0, 0, 1, z,
         0, 0, 0, 1)

  /**
   * Returns a translate matrix
   * @param t the translation vector
   */
  @inline
  def translate(t: Vec3): Mat4 =
    translate(t.x, t.y, t.z)

  @inline
  def frustum(left: Float, right: Float, bottom: Float, top: Float, zNear: Float, zFar: Float): Mat4 = {
    val A = (right + left)/(right - left)
    val B = (top + bottom)/(top - bottom)
    val C = (zFar + zNear)/(zFar - zNear)
    val D = (2*zFar*zNear)/(zFar - zNear)
    Mat4((2*zNear*zFar)/(right - left), 0,                         A, 0,
         0,                             (2*zNear)/(top - bottom),  B, 0,
         0,                             0,                         C, D,
         0,                             0,                        -1, 0)
  }

  /**
   * Construct a perspective projection matrix
   * @param fovy the fov of the y, in radians.
   * @param aspect the aspect ratio of the viewport
   * @param zNear the near plane position
   * @param zFar the far plane position
   * @note as the near plane approaches 0, depth buffer precision approaches 0.
   */
  @inline
  def perspective(fovy: Float, aspect: Float, zNear: Float, zFar: Float): Mat4 = {
    val f = 1.0f / Math.tan(fovy / 2).toFloat
    val nf = 1 / (zNear-zFar)

    Mat4(f/aspect, 0,                   0,                 0,
         0,        f,                   0,                 0,
         0,        0,                   (zFar + zNear)*nf, 2*(zFar*zNear)*nf,
         0,        0,                   -1,                0)
  }

  /**
   * Constructs an orthographic projection martix
   * @param left the left plane position
   * @param right the right plane position
   * @param bottom the bottom plane position
   * @param top the top plane position
   * @param zNear the near plane position
   * @param zFar the far plane position
   */
  @inline
  def ortho(left: Float, right: Float, bottom: Float, top: Float, zNear: Float = -1, zFar: Float = 1): Mat4 = {
    val tx = -(right + left)/(right - left)
    val ty = -(top + bottom)/(top - bottom)
    val tz = -(zFar + zNear)/(zFar - zNear)

    Mat4(2/(right-left), 0,               0,                tx,
         0,              2/(top-bottom),  0,                ty,
         0,              0,              -2/(zFar - zNear), tz,
         0,              0,               0,                1)
  }

  /**
   * Constructs a rotation and trnslation matrix that positions at the eye vector,
   * looking at the center vector, with the given up vector.
   *
   * None of these vectors need normalization.
   *
   * @param eye the eye vector - the position we are at.
   * @param center the center vector - the point we are looking at.
   * @param up the up vector - which direction points up.
   */
  @inline
  def lookAt(eye: Vec3, center: Vec3, up: Vec3): Mat4 = {
    val f = (center - eye).normalized
    val s = f ⨯ up.normalized
    val u = s.normalized ⨯ f
    Mat4( s.x,  s.y,  s.z, 0,
          u.x,  u.y,  u.z, 0,
         -f.x, -f.y, -f.z, 0,
          0,    0,    0,   1) * translate(-eye)
  }
}
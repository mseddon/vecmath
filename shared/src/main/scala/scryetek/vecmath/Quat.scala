package scryetek.vecmath

@inline
final class Quat(var x: Float, var y: Float, var z: Float, var w: Float) {
  @inline
  def set(x: Float = this.x, y: Float = this.y, z: Float = this.z, w: Float = this.w) = {
    this.x = x
    this.y = y
    this.z = z
    this.w = w
    this
  }

  @inline
  def set(q: Quat): Quat =
    set(q.x, q.y, q.z, q.w)

  @inline
  def +(q: Quat): Quat =
    Quat(x + q.x, y + q.y, z + q.z, w + q.w)

  @inline
  def add(q: Quat, out: Quat = this): Quat =
    out.set(x + q.x, y + q.y, z + q.z, w + q.w)

  @inline
  def add(x: Float, y: Float, z: Float, w: Float, out: Quat): Quat =
    out.set(this.x + x, this.y + y, this.z + z, this.w + w)

  @inline
  def add(x: Float, y: Float, z: Float, w: Float): Quat =
    add(x, y, z, w, this)

  @inline
  def -(q: Quat): Quat =
    Quat(x - q.x, y - q.y, z - q.z, w - q.w)


  @inline
  def sub(q: Quat, out: Quat = this): Quat =
    out.set(x - q.x, y - q.y, z - q.z, w - q.w)

  @inline
  def sub(x: Float, y: Float, z: Float, w: Float, out: Quat): Quat =
    out.set(this.x - x, this.y - y, this.z - z, this.w - w)

  @inline
  def sub(x: Float, y: Float, z: Float, w: Float): Quat =
    sub(x, y, z, w, this)

  @inline
  def *(q: Quat): Quat =
    Quat(x*q.w + w*q.x + y*q.z - z*q.y,
         y*q.w + w*q.y + z*q.x - x*q.z,
         z*q.w + w*q.z + x*q.y - y*q.x,
         w*q.w - x*q.x - y*q.y - z*q.z)

  @inline
  def postMultiply(q: Quat, out: Quat = this): Quat =
    out.set(x*q.w + w*q.x + y*q.z - z*q.y,
            y*q.w + w*q.y + z*q.x - x*q.z,
            z*q.w + w*q.z + x*q.y - y*q.x,
            w*q.w - x*q.x - y*q.y - z*q.z)

  @inline
  def *(v: Vec4): Vec4 =
    (this * Quat(v.x, v.y, v.z, v.w) * this.conjugated).toVec4

  @inline
  def *(v: Vec3): Vec3 = {
    val ox = x + w*v.x + y*v.z - z*v.y
    val oy = y + w*v.y + z*v.x - x*v.z
    val oz = z + w*v.z + x*v.y - y*v.x
    val ow = w - x*v.x - y*v.y - z*v.z

    val s = 1/(ow*w - ox * -x - oy * -y - oz * -z)
    Vec3((ox*w + ow * -x + oy * -z - oz * -y)*s,
         (oy*w + ow * -y + oz * -x - ox * -z)*s,
         (oz*w + ow * -z + ox * -y - oy * -x)*s)
  }

  @inline
  def mul(v: Vec4, out: Vec4) = {
    val ox = x*v.w + w*v.x + y*v.z - z*v.y
    val oy = y*v.w + w*v.y + z*v.x - x*v.z
    val oz = z*v.w + w*v.z + x*v.y - y*v.x
    val ow = w*v.w - x*v.x - y*v.y - z*v.z

    out.set(ox*w + ow * -x + oy * -z - oz * -y,
            oy*w + ow * -y + oz * -x - ox * -z,
            oz*w + ow * -z + ox * -y - oy * -x,
            ow*w - ox * -x - oy * -y - oz * -z)
  }
  
  def mul(v: Vec4): Vec4 =
    mul(v, v)

  @inline
  def mul(v: Vec3, out: Vec3) = {
    val ox = x + w*v.x + y*v.z - z*v.y
    val oy = y + w*v.y + z*v.x - x*v.z
    val oz = z + w*v.z + x*v.y - y*v.x
    val ow = w - x*v.x - y*v.y - z*v.z

    val s = 1/(ow*w - ox * -x - oy * -y - oz * -z)
    out.set((ox*w + ow * -x + oy * -z - oz * -y)*s,
            (oy*w + ow * -y + oz * -x - ox * -z)*s,
            (oz*w + ow * -z + ox * -y - oy * -x)*s)
  }



  @inline
  def *(s: Float): Quat =
    Quat(x*s, y*s, z*s, w*s)

  @inline
  def /(s: Float): Quat =
    this*(1/s)

  @inline
  def dot(q: Quat): Float =
    x*q.x + y*q.y + z*q.z + w*q.w

  @inline
  def toAngleAxis: AngleAxis = {
    val s = math.sqrt(1-w*w).toFloat
    if(s < 0.001) {
      // angle is very small and we risk dividing by zero. return identity rotation instead.
      AngleAxis(0, Vec3(1, 0, 0))
    } else {
      val s2 = 1/s
      AngleAxis(2 * math.acos(w).toFloat, Vec3(x * s2, y * s2, z * s2))
    }
  }

  /**
   * Converts this (normalized) quaternion into a matrix.
   */
  @inline
  def toMat4 = {
    val xx = x*x; val xy = x*y; val xz = x*z; val xw = x*w; val ww = w*w;
    val yy = y*y; val yz = y*z; val yw = y*w; val zz = z*z; val zw = z*w;
    Mat4(
      1-2*yy-2*zz, 2*(xy-zw),   2*(xz+yw),   0,
      2*(xy+zw),   1-2*xx-2*zz, 2*(yz-xw),   0,
      2*(xz-yw),   2*(yz+xw),   1-2*xx-2*yy, 0,
      0,           0,           0,           1)
  }

  /**
   * Converts this (normalized) quaternion into a matrix.
   */
  @inline
  def toMat3 = {
    val xx = x*x; val xy = x*y; val xz = x*z; val xw = x*w; val ww = w*w;
    val yy = y*y; val yz = y*z; val yw = y*w; val zz = z*z; val zw = z*w;
    Mat3(
      1-2*yy-2*zz, 2*(xy-zw),   2*(xz+yw),
      2*(xy+zw),   1-2*xx-2*zz, 2*(yz-xw),
      2*(xz-yw),   2*(yz+xw),   1-2*xx-2*yy)
  }

  @inline
  def conjugated =
    Quat(-x, -y, -z, w)

  @inline
  def toVec4 =
    Vec4(x, y, z, w)

  @inline
  def magSqr = x*x + y*y + z*z + w*w

  @inline
  def magnitude = math.sqrt(magSqr).toFloat

  @inline
  def normalized: Quat =
    this / magnitude

  @inline
  def lerp(q: Quat, t: Float): Quat =
    Quat(x + t*(q.x-x),
         y + t*(q.y-y),
         z + t*(q.z-z),
         w + t*(q.w-w))

  @inline
  def lerp(q: Quat, t: Float, out: Quat): Quat =
    out.set(x + t*(q.x-x),
            y + t*(q.y-y),
            z + t*(q.z-z),
            w + t*(q.w-w))

  def slerp(q: Quat, t: Float): Quat =
    slerp(q, t, Quat())

  def slerp(q: Quat, t: Float, out: Quat): Quat = {
    // another shameless crib from http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/slerp
    var qx = q.x; var qy = q.y; var qz = q.z; var qw = q.w
    // Calculate angle between them.
    var cosHalfTheta = w*qw + x*qx + y*qy + z*qz
    if(cosHalfTheta < 0) {
      qw = -qw; qx = -qx; qy = -qy; qz = qz
      cosHalfTheta = -cosHalfTheta
    }
    // if qa=qb or qa=-qb then theta = 0 and we can return qa
    if (math.abs(cosHalfTheta) >= 1.0)
      out.set(x, y, z, w)
    else {
      // Calculate temporary values.
      val halfTheta = math.acos(cosHalfTheta).toFloat
      val sinHalfTheta = math.sqrt(1.0 - cosHalfTheta * cosHalfTheta).toFloat
      // if theta = 180 degrees then result is not fully defined
      // we could rotate around any axis normal to qa or qb
      if (math.abs(sinHalfTheta) < 0.001f)
        out.set(x*0.5f + qx*0.5f, y*0.5f + qy*0.5f, z*0.5f + qz*0.5f, w*0.5f + qw*0.5f)
      else {
        val ratioA = math.sin((1 - t) * halfTheta).toFloat / sinHalfTheta
        val ratioB = math.sin(t * halfTheta).toFloat / sinHalfTheta
        //calculate Quaternion.
        out.set(x * ratioA + qx * ratioB, y * ratioA + qy * ratioB, z * ratioA + qz * ratioB, w * ratioA + qw * ratioB)
      }
    }
  }

  def copy(x: Float = x, y: Float = y, z: Float = z, w: Float = w) =
    Quat(x,y,z,w)

  override def toString =
    s"Quat(${x}f,${y}f,${z}f,${w}f)"

  override def equals(o: Any): Boolean = o match {
    case v: Quat => x == v.x && y == v.y && z == v.z && w == v.w
    case _ => false
  }

  override def hashCode: Int =
    x.hashCode() * 31 +  y.hashCode() * 53 + z.hashCode() + w.hashCode() * 71
}

object Quat {
  def apply() =
    new Quat(0, 0, 0, 1)

  def apply(x: Float, y: Float, z: Float, w: Float) =
    new Quat(x, y, z, w)

  def fromAngleAxis(angle: Float, x: Float, y: Float, z: Float): Quat = {
    val s = math.sin(angle/2).toFloat
    Quat(x*s, y*s, z*s, math.cos(angle/2).toFloat)
  }

  def fromAngleAxis(angle: Float, axis: Vec3): Quat =
    fromAngleAxis(angle, axis.x, axis.y, axis.z)
}

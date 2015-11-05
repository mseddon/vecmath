package scryetek.vecmath

import org.scalacheck._
import org.scalacheck.Prop._

object MatSpecification extends Properties("Matrix") {
  property("Mat4 from affine transforms are invertable") = forAll { (m: Mat4, v: Vec3) =>
    val v2 = Vec4(v.x, v.y, v.z, Math.random().toFloat*5)
    m * m.inverted * v2 ~= v2
  }

  property("Mat4.rotate.toQuat * v == Quat.fromAngleAxis * v") = forAll { (a: AngleAxis, v: Vec3) =>
    val v2 = Vec4(v.x, v.y, v.z, Math.random().toFloat*5)
    // the actual quaternion will NOT be the same, so we must test the effect of the transformation.
    a.toQuat * v2 ~= Mat4.rotate(a.angle, a.axis).toQuat * v2
  }

  property("Mat3.rotate.toQuat * v == Quat.fromAngleAxis * v") = forAll { (a: AngleAxis, v: Vec3) =>
    val v2 = Vec4(v.x, v.y, v.z, 1)
    // the actual quaternion will NOT be the same, so we must test the effect of the transformation.
    a.toQuat * v2 ~= Mat3.rotate(a.angle, a.axis).toQuat * v2
  }

  property("Mat3.rotate * v == Quat.fromAngleAxis.toMat3 * v") = forAll { (a: AngleAxis, v: Vec3) =>
    // the actual quaternion will NOT be the same, so we must test the effect of the transformation.
    a.toQuat.toMat3 * v ~= Mat3.rotate(a.angle, a.axis) * v
  }

  property("Mat4().adjoint == Mat4()") = propBoolean {
    Mat4().adjointed == Mat4()
  }

  property("Mat2().adjoint == Mat2()") = propBoolean {
    Mat2().adjointed == Mat2()
  }

  property("Mat4: A*/det(A) = A^-1*") = forAll { (m1: Mat4, v: Vec3) =>
    val v2 = Vec4(v.x, v.y, v.z, 1)
    // this is not very numerically pleasant.
    (m1.adjointed * (1/m1.determinant) * v2).approximatelyEqualTo(m1.inverted*v2,0.005f)
  }

  property("Mat3: A*/det(A) = A^-1*") = forAll { (m1: Mat3, v: Vec2) =>
    val v2 = Vec3(v.x, v.y, 1)
    // this is not very numerically pleasant.
    (m1.adjointed * (1/m1.determinant) * v2).approximatelyEqualTo(m1.inverted*v2,0.005f)
  }

  property("Mat2: A*/det(A) = A^-1*") = forAll { (m1: Mat2, v: Vec2) =>
    // this is not very numerically pleasant.
    (m1.adjointed * (1/m1.determinant) * v).approximatelyEqualTo(m1.inverted*v,0.05f)
  }

  property("Every Mat2d performs an equivalent transformation to it's respective Mat3") = forAll { (m: Mat2d, v: Vec3) =>
    m * v ~= m.toMat3 * v
  }

  property("Every Mat2d performs an equivalent transformation to it's respective Mat4") = forAll { (m: Mat2d, v: Vec3) =>
    val v4 = m.toMat4 * Vec4(v.x, v.y, v.z, 1)
    m * v ~= Vec3(v4.x, v4.y, v4.z)
  }

  property("Every Mat2d's inverse performs the equivalent transform as it's respective Mat3's inverse") = forAll { (m: Mat2d, v: Vec3) =>
    m.inverted * v ~= m.toMat3.inverted * v
  }


  property("Every Mat2d's determinant is the same as it's respective Mat3's determinant") = forAll { (m: Mat2d ) =>
    m.determinant ~= m.toMat3.determinant
  }


  /// Mutable specifications
  property("Mat2.preMultiply is the same as M2 * M1") = forAll { (m1: Mat2, m2: Mat2) =>
    val outMat = Mat2()
    m1.preMultiply(m2,outMat) == m2 * m1
  }

  property("Mat2.postMultiply is the same as M1 * M2") = forAll { (m1: Mat2, m2: Mat2) =>
    val outMat = Mat2()
    m1.postMultiply(m2,outMat) == m1 * m2
  }

  property("Mat2.transpose works the same as Mat2.transposed") = forAll { (m: Mat2) =>
    val outMat = Mat2()
    m.transpose(outMat) == m.transposed
  }

  property("Mat2.invert works the same as Mat2.inverted") = forAll { (m: Mat2) =>
    val outMat = Mat2()
    m.invert(outMat) == m.inverted
  }

  property("Mat2.adjointInto works the same as Mat2.adjoint") = forAll { (m: Mat2) =>
    val outMat = Mat2()
    m.adjoint(outMat) == m.adjointed
  }

  property("Mat2.add is the same as M1 + M2") = forAll { (m1: Mat2, m2: Mat2) =>
    val outMat = Mat2()
    m1.add(m2,outMat) == m2 + m1
  }

  property("Mat2.sub is the same as M1 - M2") = forAll { (m1: Mat2, m2: Mat2) =>
    val outMat = Mat2()
    m1.sub(m2,outMat) == m1 - m2
  }

  property("Mat2 M1 + M2 - M1 == M2") = forAll { (m1: Mat2, m2: Mat2, v: Vec2) =>
    (m1 + m2 - m1) * v approximatelyEqualTo (m2 * v, 0.01f)
  }


  property("Mat2.mul(v) is the same as M * v") = forAll { (m1: Mat2, v: Vec2) =>
    val outVec = Vec2()
    m1.mul(v, outVec) ~= m1 * v
  }

  property("Mat2.postMultiply is the same as M1 * M2") = forAll { (m1: Mat2d, m2: Mat2d) =>
    val outMat = Mat2d()
    m1.postMultiply(m2,outMat) == m1 * m2
  }

  property("Mat2.preMultiply is the same as M2 * M1") = forAll { (m1: Mat2d, m2: Mat2d) =>
    val outMat = Mat2d()
    m1.preMultiply(m2,outMat) == m2 * m1
  }

  property("Mat2d.invert works the same as Mat2d.inverted") = forAll { (m1: Mat2d) =>
    val outMat = Mat2d()
    m1.invert(outMat) == m1.inverted
  }

  property("Mat2d.add is the same as M1 + M2") = forAll { (m1: Mat2d, m2: Mat2d) =>
    val outMat = Mat2d()
    m1.add(m2,outMat) == m2 + m1
  }

  property("Mat2d.sub is the same as M1 - M2") = forAll { (m1: Mat2d, m2: Mat2d) =>
    val outMat = Mat2d()
    m1.sub(m2,outMat) == m1 - m2
  }

  property("Mat2d M1 + M2 - M1 == M2") = forAll { (m1: Mat2d, m2: Mat2d, v: Vec3) =>
    (m1 + m2 - m1) * v approximatelyEqualTo (m2 * v, 0.01f)
  }

  property("Mat2d.mul(Vec2) is the same as M * Vec2") = forAll { (m1: Mat2d, v: Vec2) =>
    val outVec = Vec2()
    m1.mul(v, outVec) ~= m1 * v
  }

  property("Mat2d.mul(Vec3) is the same as M * Vec3") = forAll { (m1: Mat2d, v: Vec3) =>
    val outVec = Vec3()
    m1.mul(v, outVec) ~= m1 * v
  }

  // Mat3

  property("Mat3.transpose works the same as Mat3.transposed") = forAll { (m1: Mat3) =>
    val outMat = Mat3()
    m1.transpose(outMat) == m1.transposed
  }

  property("Mat3.invert works the same as Mat3.inverted") = forAll { (m1: Mat3) =>
    val outMat = Mat3()
    m1.invert(outMat) == m1.inverted
  }

  property("Mat3.adjointInto works the same as Mat3.adjoint") = forAll { (m1: Mat3) =>
    val outMat = Mat3()
    m1.adjoint(outMat) == m1.adjointed
  }

  property("Mat3.add is the same as M1 + M2") = forAll { (m1: Mat3, m2: Mat3) =>
    val outMat = Mat3()
    m1.add(m2,outMat) == m2 + m1
  }

  property("Mat3.sub is the same as M1 - M2") = forAll { (m1: Mat3, m2: Mat3) =>
    val outMat = Mat3()
    m1.sub(m2,outMat) == m1 - m2
  }

  property("Mat3 M1 + M2 - M1 == M2") = forAll { (m1: Mat3, m2: Mat3, v: Vec3) =>
    (m1 + m2 - m1) * v approximatelyEqualTo (m2 * v, 0.01f)
  }

  property("Mat3.mul(v) is the same as M * v") = forAll { (m1: Mat3, v: Vec3) =>
    val outVec = Vec3()
    m1.mul(v, outVec) ~= m1 * v
  }

  property("Mat3.postMultiply is the same as M1 * M2") = forAll { (m1: Mat3, m2: Mat3) =>
    val outMat = Mat3()
    m1.postMultiply(m2,outMat) == m1 * m2
  }

  property("Mat3.preMultiply is the same as M2 * M1") = forAll { (m1: Mat3, m2: Mat3) =>
    val outMat = Mat3()
    m1.preMultiply(m2,outMat) == m2 * m1
  }

  property("Mat3.rotate * m == Mat.preMultiply(rotate)") = forAll { (m: Mat3, a: AngleAxis) =>
    Mat3.rotate(a.angle, a.axis) * m == m.preRotate(a.angle, a.axis)
  }

  property("m * Mat3.rotate == Mat.preMultiply(rotate)") = forAll { (m: Mat3, a: AngleAxis) =>
    m * Mat3.rotate(a.angle, a.axis) == m.postRotate(a.angle, a.axis)
  }

  property("Mat3.scale * m is equivalent to Mat.preScale(v)") = forAll { (m: Mat3, v: Vec3) =>
    Mat3.scale(v) * m == m.preScale(v)
  }

  property("m * Mat3.scale is equivalent to Mat.postScaley(v)") = forAll { (m: Mat3, v: Vec3) =>
    m * Mat3.scale(v) == m.postScale(v)
  }

  property("Mat3.rotateX == Mat.rotate(x, 1, 0, 0)") = forAll { (angle: Float, v: Vec3) =>
    Mat3.rotateX(angle) * v ~= Mat3.rotate(angle, 1, 0, 0) * v
  }

  property("Mat3.rotateX * m == Mat.preRotateX") = forAll { (angle: Float, v: Vec3, m: Mat3) =>
    Mat3.rotateX(angle) * m * v ~= m.preRotateX(angle) * v
  }

  property("m * Mat3.rotateX == Mat.postRotateX") = forAll { (angle: Float, v: Vec3, m: Mat3) =>
    m * Mat3.rotateX(angle) * v ~= m.postRotateX(angle) * v
  }


  property("Mat3.rotateY == Mat.rotate(a, 0, 1, 0)") = forAll { (angle: Float, v: Vec3) =>
    Mat3.rotateY(angle) * v ~= Mat3.rotate(angle, 0, 1, 0) * v
  }

  property("Mat3.rotateY * m == Mat.preRotateY") = forAll { (angle: Float, v: Vec3, m: Mat3) =>
    Mat3.rotateY(angle) * m * v ~= m.preRotateY(angle) * v
  }

  property("m * Mat3.rotateY == Mat.postRotateY") = forAll { (angle: Float, v: Vec3, m: Mat3) =>
    m * Mat3.rotateY(angle) * v ~= m.postRotateY(angle) * v
  }

  property("Mat3.rotateZ == Mat.rotate(a, 0, 0, 1)") = forAll { (angle: Float, v: Vec3) =>
    Mat3.rotateZ(angle) * v ~= Mat3.rotate(angle, 0, 0, 1) * v
  }

  property("Mat3.rotateZ * m == Mat.preRotateZ") = forAll { (angle: Float, v: Vec3, m: Mat3) =>
    Mat3.rotateZ(angle) * m * v ~= m.preRotateZ(angle) * v
  }

  property("m * Mat3.rotateZ == Mat.postRotateZ") = forAll { (angle: Float, v: Vec3, m: Mat3) =>
    m * Mat3.rotateZ(angle) * v ~= m.postRotateZ(angle) * v
  }

  property("m * q == m * q.toMat3") = forAll { (m: Mat3, q: Quat) =>
    m * q == m * q.toMat3
  }

  property("m * q == Mat3.postRotateQuat(q)") = forAll { (m: Mat3, q: Quat) =>
    m * q == m.postRotateQuat(q)
  }

  property("q * m == Mat3.postRotateQuat(q)") = forAll { (m: Mat3, q: Quat) =>
    m * q == m.postRotateQuat(q)
  }

  property("q.toMat3 == Mat3.rotateQuat") = forAll { (q: Quat) =>
    q.toMat3 == Mat3.rotateQuat(q)
  }
  // Mat4

  property("Mat4.transpose works the same as Mat4.transposed") = forAll { (m1: Mat4) =>
    val outMat = Mat4()
    m1.transpose(outMat) == m1.transposed
  }

  property("Mat4.invert works the same as Mat3.inverted") = forAll { (m: Mat4) =>
    val outMat = Mat4()
    m.invert(outMat) == m.inverted
  }

  property("Mat4.adjointInto works the same as Mat3.adjoint") = forAll { (m: Mat4) =>
    val outMat = Mat4()
    m.adjointed(outMat) == m.adjointed
  }

  property("Mat4.postMultiply is the same as M1 * M2") = forAll { (m1: Mat4, m2: Mat4) =>
    val outMat = Mat4()
    m1.postMultiply(m2,outMat) == m1 * m2
  }

  property("Mat4.preMultiply is the same as M2 * M1") = forAll { (m1: Mat4, m2: Mat4) =>
    val outMat = Mat4()
    m1.preMultiply(m2,outMat) == m2 * m1
  }

  property("Mat4.add is the same as M1 + M2") = forAll { (m1: Mat4, m2: Mat4) =>
    val outMat = Mat4()
    m1.add(m2,outMat) == m1 + m2
  }

  property("Mat4.sub is the same as M1 - M1") = forAll { (m1: Mat4, m2: Mat4) =>
    val outMat = Mat4()
    m1.sub(m2,outMat) == m1 - m2
  }

  property("Mat4.mul(v) is the same as M * v") = forAll { (m1: Mat4, v: Vec4) =>
    val outVec = Vec4()
    m1.mul(v, outVec) ~= m1 * v
  }

  property("Mat4.rotate * m == Mat.preMultiply(rotate)") = forAll { (m: Mat4, a: AngleAxis) =>
    Mat4.rotate(a.angle, a.axis) * m == m.preRotate(a.angle, a.axis)
  }

  property("m * Mat4.rotate == Mat.preMultiply(rotate)") = forAll { (m: Mat4, a: AngleAxis) =>
    m * Mat4.rotate(a.angle, a.axis) == m.postRotate(a.angle, a.axis)
  }

  property("Mat4.scale * m is equivalent to Mat.preScale(v)") = forAll { (m: Mat4, v: Vec3) =>
    Mat4.scale(v) * m == m.preScale(v)
  }

  property("m * Mat4.scale is equivalent to Mat.postScale(v)") = forAll { (m: Mat4, v: Vec3) =>
    m * Mat4.scale(v) == m.postScale(v)
  }

  property("Mat4.translate * m is equivalent to Mat.preTranslate(v)") = forAll { (m: Mat4, v: Vec3) =>
    Mat4.translate(v) * m == m.preTranslate(v)
  }

  property("m * Mat4.translate is equivalent to Mat.postTranslate(v)") = forAll { (m: Mat4, v: Vec3) =>
    m * Mat4.translate(v) == m.postTranslate(v)
  }

  property("Mat4.rotateX == Mat.rotate(x, 1, 0, 0)") = forAll { (angle: Float, v: Vec4) =>
    Mat4.rotateX(angle) * v ~= Mat4.rotate(angle, 1, 0, 0) * v
  }

  property("Mat4.rotateX * m == Mat.preRotateX") = forAll { (angle: Float, v: Vec4, m: Mat4) =>
    Mat4.rotateX(angle) * m * v ~= m.preRotateX(angle) * v
  }

  property("m * Mat4.rotateX == Mat.postRotateX") = forAll { (angle: Float, v: Vec4, m: Mat4) =>
    m * Mat4.rotateX(angle) * v ~= m.postRotateX(angle) * v
  }


  property("Mat4.rotateY == Mat.rotate(a, 0, 1, 0)") = forAll { (angle: Float, v: Vec4) =>
    Mat4.rotateY(angle) * v ~= Mat4.rotate(angle, 0, 1, 0) * v
  }

  property("Mat4.rotateY * m == Mat.preRotateY") = forAll { (angle: Float, v: Vec4, m: Mat4) =>
    Mat4.rotateY(angle) * m * v ~= m.preRotateY(angle) * v
  }

  property("m * Mat4.rotateY == Mat.postRotateY") = forAll { (angle: Float, v: Vec4, m: Mat4) =>
    m * Mat4.rotateY(angle) * v ~= m.postRotateY(angle) * v
  }

  property("Mat4.rotateZ == Mat.rotate(a, 0, 0, 1)") = forAll { (angle: Float, v: Vec4) =>
    Mat4.rotateZ(angle) * v ~= Mat4.rotate(angle, 0, 0, 1) * v
  }

  property("Mat4.rotateZ * m == Mat.preRotateZ") = forAll { (angle: Float, v: Vec4, m: Mat4) =>
    Mat4.rotateZ(angle) * m * v ~= m.preRotateZ(angle) * v
  }

  property("m * Mat4.rotateZ == Mat.postRotateZ") = forAll { (angle: Float, v: Vec4, m: Mat4) =>
    m * Mat4.rotateZ(angle) * v ~= m.postRotateZ(angle) * v
  }

  property("m * q == m * q.toMat4") = forAll { (m: Mat4, q: Quat) =>
    m * q == m * q.toMat4
  }

  property("m * q == Mat4.postRotate(q)") = forAll { (m: Mat4, q: Quat) =>
    m * q == m.postRotateQuat(q)
  }

  property("q * m == Mat4.preRotate(q)") = forAll { (m: Mat4, q: Quat) =>
    m * q == m.postRotateQuat(q)
  }

  property("q.toMat4 == Mat4.rotateQuat") = forAll { (q: Quat) =>
    q.toMat4 == Mat4.rotateQuat(q)
  }

  property("Mat4.toAngleAxis.toMat4 == ") = forAll { (a: AngleAxis, v: Vec4) =>
    val a2 = Mat4.rotate(a.angle, a.axis).toAngleAxis
    val v0 = Mat4.rotate(a.angle, a.axis) * v
    val v1 = Mat4.rotate(a2.angle, a2.axis) * v
    v0 approximatelyEqualTo (v1, 1f)
  }


  property("Mat4 M1 + M2 - M1 == M2") = forAll { (m1: Mat4, m2: Mat4, v: Vec4) =>
    (m1 + m2 - m1) * v approximatelyEqualTo (m2 * v, 0.01f)
  }
}


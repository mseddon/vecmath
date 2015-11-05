package scryetek.vecmath

import org.scalacheck._
import org.scalacheck.Prop._

object QuatSpecification extends Properties("Quat") {
  property("Quat angle axis to matrix is equivalent to standard rotation matrix") = forAll { (m: AngleAxis, v: Vec4) =>
    (Mat4.rotate(m.angle, m.axis) * v) ~= (Quat.fromAngleAxis(m.angle, m.axis).toMat4 * v)
  }

  property("Quat * Vec4 is equivalent to rotation matrix * Vec4") = forAll { (m: AngleAxis, v: Vec4) =>
    (Mat4.rotate(m.angle, m.axis) * v) ~= (Quat.fromAngleAxis(m.angle, m.axis) * v)
  }

  property("Q1 * Q2 * v == M1 * M2 * v for equivalent rotation matrices M") = forAll { (q: Quat, m: AngleAxis, m2: AngleAxis, v: Vec4) =>
    (Mat4.rotate(m.angle, m.axis) * Mat4.rotate(m2.angle, m2.axis) * v) ~= (Quat.fromAngleAxis(m.angle, m.axis) * Quat.fromAngleAxis(m2.angle, m2.axis) * v)
  }

  property("Quat * Quat.conjugate * v == v") = forAll { (q: Quat, v: Vec4) =>
    q * q.conjugated * v ~= v
  }

  property("q * vec3 is the same as a q * vec4 with w = 1") = forAll { (q: Quat, v: Vec3) =>
    val v4 = q * Vec4(v.x, v.y, v.z, 1)
    q * v approximatelyEqualTo (Vec3(v4.x, v4.y, v4.z), 0.005f)
  }

  // Mutable versions
  property("q.mul(v) is equivalent to q*v for Vec4") = forAll { (q: Quat, v: Vec4) =>
    val vOut = Vec4()
    val vOld = v.copy()
    (q * v ~= q.mul(v, vOut)) && (vOld ~= v)
  }

  property("q.mul(v) is equivalent to q*v for Vec3") = forAll { (q: Quat, v: Vec3 ) =>
    val vOut = Vec3()
    val vOld = v.copy()
    (q * v ~= q.mul(v, vOut)) && (vOld ~= v)
  }

  property("q1.postMultiply(q2) is equivalent to q1 * q1") = forAll { (q1: Quat, q2: Quat) =>
    val qOut = Quat()
    val qOld = q1.copy()
    (q1 * q2 == q1.postMultiply(q2, qOut)) && (qOld == q1)
  }

  property("Quat.add(Quat) is equivalent to Quat + Quat") = forAll { (q1: Quat, q2: Quat) =>
    val qOut = Quat()
    val qOld = q1.copy()
    (q1 + q2 == q1.add(q2, qOut)) && (qOld == q1)
  }

  property("Quat.sub(Quat) is equivalent to Quat - Quat") = forAll { (q1: Quat, q2: Quat) =>
    val qOut = Quat()
    val qOld = q1.copy()
    (q1 - q2 == q1.sub(q2, qOut)) && (qOld == q1)
  }

  property("q1.slerp(q2,1) * v == q2 * v") = forAll { (q1: Quat, q2: Quat, v: Vec3) =>
    (q1.slerp(q2, 0) * v) approximatelyEqualTo (q1 * v, 0.01f)
  }

  property("q1.slerp(q2,0) * v == q1 * v") = forAll { (q1: Quat, q2: Quat, v: Vec3) =>
    (q1.slerp(q2, 0) * v) approximatelyEqualTo (q1 * v, 0.01f)
  }


}

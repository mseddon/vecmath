package scryetek

import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
/**
 * Created by Matt on 01/11/2015.
 */
package object vecmath {
  implicit val arbitraryVec3 = Arbitrary(for {
    x <- Gen.choose(-100f, 100f)
    y <- Gen.choose(-100f, 100f)
    z <- Gen.choose(-100f, 100f)
    vec = Vec3(x,y,z) if !vec.magnitude.isInfinite && !vec.magnitude.isNaN
  } yield vec)

  implicit val arbitraryVec2 = Arbitrary(for {
    x <- Gen.choose(-100f, 100f)
    y <- Gen.choose(-100f, 100f)
    vec = Vec2(x,y) if !vec.magnitude.isInfinite && !vec.magnitude.isNaN
  } yield vec)

  implicit val arbitraryAngleAxis = Arbitrary(for {
    v <- arbitrary[Vec3]
    angle <- Gen.choose(-math.Pi*2, math.Pi*2)
  } yield AngleAxis(angle.toFloat, v.normalized))

  implicit val arbitraryVec4 = Arbitrary(for {
    x <- Gen.choose(-100f, 100f)
    y <- Gen.choose(-100f, 100f)
    z <- Gen.choose(-100f, 100f)
    w <- Gen.choose(-100f, 100f)
    vec = Vec4(x,y,z,w) if !vec.magnitude.isInfinite && !vec.magnitude.isNaN
  } yield vec)

  implicit val arbitraryQuat = Arbitrary(for {
    axis <- arbitrary[Vec3]
    angle <- Gen.choose(-math.Pi*2, math.Pi*2)
  } yield Quat.fromAngleAxis(angle.toFloat, axis.normalized))

  implicit val arbitraryMat4 = Arbitrary(for {
    aa <- arbitrary[AngleAxis]
    scale <- arbitrary[Vec3] if scale.x != 0 && scale.y != 0 && scale.z != 0
    translate <- arbitrary[Vec3]
  } yield Mat4.rotate(aa.angle, aa.axis) * Mat4.translate(translate) * Mat4.scale(scale))

  implicit val arbitraryMat3 = Arbitrary(for {
    aa <- arbitrary[AngleAxis]
    scale <- arbitrary[Vec3] if scale.x != 0 && scale.y != 0 && scale.z != 0
  } yield Mat3.rotate(aa.angle, aa.axis) * Mat3.scale(scale))

  implicit val arbitraryMat2 = Arbitrary(for {
    angle <- Gen.choose(-math.Pi*2, math.Pi*2)
    scale <- arbitrary[Vec2] if scale.x != 0 && scale.y != 0
  } yield Mat2.rotate(angle.toFloat) * Mat2.scale(scale))

  implicit val arbitraryMat2d = Arbitrary(for {
    m <- arbitrary[Mat3]
  } yield m.toMat2d)

  implicit class floatPimp(val f: Float) extends AnyVal {
    def approximatelyEqualTo(x: Float, epsilon: Float) =
      math.abs(f-x) < epsilon

    def ~=(x: Float) =
      this.approximatelyEqualTo(x, 0.001f)
  }

  implicit class vec2Pimp(val v: Vec2) {
    @inline
    def approximatelyEqualTo(v2: Vec2, epsilon: Float): Boolean =
      math.abs((v - v2).magnitude) < epsilon
    def ~=(v: Vec2): Boolean =
      this.approximatelyEqualTo(v, 0.001f)
  }

  implicit class vec3Pimp(val v: Vec3) {
    @inline
    def approximatelyEqualTo(v2: Vec3, epsilon: Float): Boolean =
      math.abs((v - v2).magnitude) < epsilon
    def ~=(v: Vec3): Boolean =
      this.approximatelyEqualTo(v, 0.001f)
  }

  implicit class vec4Pimp(val v: Vec4) {
    @inline
    def approximatelyEqualTo(v2: Vec4, epsilon: Float): Boolean =
      math.abs((v - v2).magnitude) < epsilon
    def ~=(v: Vec4): Boolean =
      this.approximatelyEqualTo(v, 0.001f)
  }

}

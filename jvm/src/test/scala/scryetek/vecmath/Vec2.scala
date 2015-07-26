package scryetek.vecmath

import org.scalatest._

/**
 * Created by matt on 08/06/15.
 */
class MatSpec extends FlatSpec with Matchers {
  val Epsilon = 0.001f

  "A Vec2" should "be correctly transformed by a translation Mat2d" in {
    val v = Vec2(0,0)
    (Mat2d().translate(Vec2(3, 4)) * v - Vec2(3, 4)).magnitude should be < Epsilon
  }

  "A Vec2" should "be correctly transformed by a translation Mat3" in {
    val v = Vec2(0,0)
    (Mat3().translate(Vec2(3, 4)) * v - Vec2(3, 4)).magnitude should be < Epsilon
  }

  "A translation Mat2d" should "be invertable" in {
    val v = Vec2(0,0)
    val m = Mat2d().translate(Vec2(3, 4))

    (m * (m.invertInto(Mat2d()) * v - Vec2(0,0))).magnitude should be < Epsilon
  }

  "A rotation Mat2d" should "be invertable" in {
    val v = Vec2(0,0)
    val m = Mat2d().rotate(Math.PI.toFloat*2)

    (m * (m.invertInto(Mat2d()) * v - Vec2(0,0))).magnitude should be < Epsilon
  }
}

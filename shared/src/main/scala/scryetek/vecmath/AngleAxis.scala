package scryetek.vecmath

@inline
case class AngleAxis(angle: Float, axis: Vec3) {
  @inline
  def toQuat = Quat.fromAngleAxis(angle, axis.x, axis.y, axis.z)
}

import MatrixConstants.Identity

import java.util.UUID

case class Sphere(origin: Point = Point(0,0,0), radius: Float = 1, id: String = UUID.randomUUID().toString) {
  var transform: Matrix = Identity()
  def setTransform(newTransform: Matrix): Unit = {
    transform = newTransform
  }

  def normalAt(worldPoint: Point): Vector = {
    val objectPoint = transform.inverse() * worldPoint
    val objectNormal = Vector(objectPoint.x, objectPoint.y, objectPoint.z)
    val worldNormal: Vector = transform.inverse().transpose() * objectNormal
    worldNormal.w = 0
    worldNormal.normalized()
  }
}


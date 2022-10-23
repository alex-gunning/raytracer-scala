import MatrixConstants.Identity

import java.util.UUID

case class Sphere(origin: Point = Point(0,0,0), radius: Float = 1, id: String = UUID.randomUUID().toString) {
  var transform: Matrix = Identity()
  def setTransform(newTransform: Matrix): Unit = {
    transform = newTransform
  }
}


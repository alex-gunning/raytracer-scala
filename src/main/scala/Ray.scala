import scala.math.{pow, sqrt}

case class RayIntersection(t: Float, obj: Sphere)
case class Ray(origin: Point, direction: Vector) {
//  def apply(j: Int) =
  def position(t: Float) = origin + direction * t

  // Object intersections
  def intersect(sphere: Sphere): Array[RayIntersection] = {
    // Transform object coords to world coords.
    val worldCoordsRay = Intersect.transform(Ray(origin, direction), sphere.transform.inverse())

    val sphereToRay = worldCoordsRay.origin - sphere.origin
    val a = worldCoordsRay.direction.dot(worldCoordsRay.direction)
    val b = 2 * worldCoordsRay.direction.dot(sphereToRay)
    val c = sphereToRay.dot(sphereToRay) - 1

    val discriminant = pow(b, 2) - 4 * a * c

    if(discriminant < 0) {
      Array()
    } else {
      val t1 = (-b - sqrt(discriminant)) / (2 * a)
      val t2 = (-b + sqrt(discriminant)) / (2 * a)

      Array(RayIntersection(t1.toFloat, sphere), RayIntersection(t2.toFloat, sphere))
    }
  }
}

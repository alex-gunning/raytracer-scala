import java.util.UUID

case class Sphere(origin: Point = Point(0,0,0), radius: Float = 1, id: String = UUID.randomUUID().toString)


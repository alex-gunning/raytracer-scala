case class Point(x: Float, y: Float, z: Float, w: Int = 1) {
  def +(other: Vector) = Point(x + other.x, y + other.y, z + other.z, w + other.w)

  def -(other: Vector) = Point(x - other.x, y - other.y, z - other.z, w - other.w)

  def -(other: Point) = Vector(x - other.x, y - other.y, z - other.z, w - other.w)
}

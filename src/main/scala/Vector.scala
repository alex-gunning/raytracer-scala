case class Vector(x: Float, y: Float, z: Float, var w: Int = 0) {
  def +(other: Vector) = Vector(x + other.x, y + other.y, z + other.z, w + other.w)

  def -(other: Vector) = Vector(x - other.x, y - other.y, z - other.z, w - other.w)

  def unary_-() = Vector(0, 0, 0) - Vector(x, y, z)

  def *(scalar: Int) = Vector(x * scalar, y * scalar, z * scalar, w * scalar)

  def *(scalar: Float) = Vector(x * scalar, y * scalar, z * scalar, (w * scalar).toInt)

  def /(scalar: Int) = Vector(x / scalar, y / scalar, z / scalar, w / scalar)

  def /(scalar: Float) = Vector(x / scalar, y / scalar, z / scalar, (w / scalar).toInt)

  def magnitude() = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2) + Math.pow(z, 2)).toFloat

  def normalized(): Vector = {
    val mag = this.magnitude()
    Vector(x / mag, y / mag, z / mag)
  }

  def dot(other: Vector) = x * other.x + y * other.y + z * other.z + w * other.z

  def cross(o: Vector) = Vector(
    y * o.z - o.y * z,
    z * o.x - x * o.z,
    x * o.y - y * o.x
  )

  def reflect(n: Vector) = this - n * 2 * this.dot(n)
}

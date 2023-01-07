
case class Projectile(position: Point, velocity: Vector)

case class Environment(gravity: Vector, wind: Vector)
import Intersect.hit
import Lighting.{PointLight, lighting}
import Transformations.{rotateY, rotateZ, scaling, shearing, translation}

import java.io.PrintWriter

object Main {
  val canvasPixels = 500
  val c = Canvas(canvasPixels, canvasPixels)

  def main(args: Array[String]): Unit = {
    println("SphereSilhouette")

//    cannonTick(Projectile(Point(0, 1, 0), Vector(1, 1.8f, 0).normalized() * 11.25f), Environment(Vector(0, -0.1f, 0), Vector(-0.01f, 0, 0)))
//    clock()
    sphereSilloette()
    new PrintWriter("SphereSilhouette.ppm") { write(c.serialize); close }
    println("Completed")
  }

  def sphereSilloette(): Unit = {
    val rayOrigin = Point(0,0,-5)
    val wallZ = 10f
    val wallSize = 7f
    val pixelSize = wallSize / canvasPixels
    val half = wallSize / 2f
    val shape = Sphere()
//    shape.setTransform(shearing(1,0,0,0,0,0) * scaling(0.5f, 1, 1))
    shape.material = Material(Colour(1f,0.2f,1f))
    val light = PointLight(Point(-10, 10, -10), Colour(1, 1, 1))

    for(y <- Range.inclusive(0, canvasPixels - 1)) {
      val worldY = half - pixelSize * y
      for(x <- Range.inclusive(0, canvasPixels - 1)) {
        val worldX = -half + pixelSize * x
        val position = Point(worldX, worldY, wallZ)

        val normalizedPosition = position - rayOrigin
        val ray = Ray(rayOrigin, Vector(normalizedPosition.x, normalizedPosition.y, normalizedPosition.z).normalized())
        val xs = ray.intersect(shape)

        val h = hit(xs)
        if(h.length > 0) {
          val hit = h(0)
          val point = ray.position(hit.t)
          val normal = hit.obj.normalAt(point)
          val eye = -ray.direction
          val colour = lighting(hit.obj.material, light, point, eye, normal)

          c.writePixel(x, y, colour)
        }
      }
    }
  }

  def clock(): Unit = {
    val twelveOclock = Point(0, 0, 1)
    val rotateMatrix = rotateY((Math.PI / 6).toFloat)
    val transformation = translation(250, 0, 250) * scaling(150, 0, 150)
    val a = transformation * twelveOclock
//    c.writePixel(a.x.toInt, a.y.toInt, Colour(1, 1, 0))
    var num = twelveOclock
    for(? <- Range.inclusive(0, 12)) {
      num = rotateMatrix * num
      val point = transformation * num
      c.writePixel(point.x.toInt, point.z.toInt, Colour(1,1,0))
    }
  }

  def cannonTick(proj: Projectile, env: Environment): Unit = {
    if (proj.position.y < 0) {
      return
    } else {
      val newPos = proj.position + proj.velocity
      val newVel = proj.velocity + env.gravity + env.wind
      c.writePixel(proj.position.x.toInt, c.h - proj.position.y.toInt, Colour(1, 1, 0))
      cannonTick(Projectile(newPos, newVel), env)
    }
  }
}
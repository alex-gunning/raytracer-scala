
case class Projectile(position: Point, velocity: Vector)

case class Environment(gravity: Vector, wind: Vector)
import Transformations.{rotateY, scaling, translation}

import java.io.PrintWriter

object Main {
  val c = Canvas(500, 500)

  def main(args: Array[String]): Unit = {
    println("Clock")

//    cannonTick(Projectile(Point(0, 1, 0), Vector(1, 1.8f, 0).normalized() * 11.25f), Environment(Vector(0, -0.1f, 0), Vector(-0.01f, 0, 0)))
    clock()
    new PrintWriter("image.ppm") { write(c.serialize); close }
    println("Completed")
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
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

case class Projectile(position: Point, velocity: Vector)

case class Environment(gravity: Vector, wind: Vector)
import java.io.PrintWriter

object Main {
  val c = Canvas(900, 550)

  def main(args: Array[String]): Unit = {
    println("Virtual cannon")

    cannonTick(Projectile(Point(0, 1, 0), Vector(1, 1.8f, 0).normalized() * 11.25f), Environment(Vector(0, -0.1f, 0), Vector(-0.01f, 0, 0)))
    new PrintWriter("image.ppm") { write(c.serialize); close }
    println("Completed")
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
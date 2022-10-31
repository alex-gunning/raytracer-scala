import Lighting.{PointLight, lighting}
import org.scalatest.funsuite.AnyFunSuite

import scala.math.sqrt

class LightingUnitTests extends AnyFunSuite {
  test("Lighting with the eye between the light and the surface") {
    val m = Material()
    val position = Point(0,0,0)

    val eyev = Vector(0,0,-1f)
    val normalv = Vector(0,0,-1f)
    val light = PointLight(Point(0,0,-10f), Colour(1,1,1))
    val result = lighting(m, light, position, eyev, normalv)
    assert(result == Colour(1.9f,1.9f,1.9f))
  }
  test("Lighting with the eye opposite surface, light offset 45 degrees") {
    val m = Material()
    val position = Point(0,0,0)

    val eyev = Vector(0,0,-1)
    val normalv = Vector(0,0,-1)
    val light = PointLight(Point(0,10,-10), Colour(1,1,1))
    val result = lighting(m, light, position, eyev, normalv)
    assert(result == Colour(0.7364f,0.7364f,0.7364f))
  }
  test("Lighting with the eye in the path of the reflection vector") {
    val m = Material()
    val position = Point(0,0,0)

    val eyev = Vector(0,-(sqrt(2)/2).toFloat,-(sqrt(2)/2).toFloat)
    val normalv = Vector(0,0,-1)
    val light = PointLight(Point(0,10,-10), Colour(1,1,1))
    val result = lighting(m, light, position, eyev, normalv)
    assert(Approx.truncateDecimals(result) == Colour(1.63639f,1.63639f,1.6363f))
  }
  test("Lighting with the eye behind the surface") {
    val m = Material()
    val position = Point(0,0,0)

    val eyev = Vector(0,0,-1)
    val normalv = Vector(0,0,-1)
    val light = PointLight(Point(0,0,10), Colour(1,1,1))
    val result = lighting(m, light, position, eyev, normalv)
    assert(result == Colour(0.1f,0.1f,0.1f))
  }
}

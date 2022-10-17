import Approx.{errorEpsilon, truncateDecimals}
import Intersect.{Intersection, hit, intersections}
import Transformations.{rotateX, rotateY, rotateZ, scaling, shearing, translation}
import org.scalatest.funsuite.AnyFunSuite

import scala.math.{Pi, sqrt}

class DrawingPrimitivesUnitTests extends AnyFunSuite {
  test("A point can be added to a vector to yield another point") {
    val point = Point(1, 2, -3)
    val vec = Vector(1, 2, 3)
    val result = point + vec
    assert(result == Point(2, 4, 0, 1))
  }

  test("Two points can be subtracted to yield a vector") {
    val point1 = Point(1, 2, -3)
    val point2 = Point(2, 1, 3)
    val result = point1 - point2
    assert(result == Vector(-1, 1, -6, 0))
  }

  test("Subtracting two vectors yields a vector") {
    val vector1 = Vector(1, 1, 1)
    val vector2 = Vector(2, 2, 3)
    val result = vector1 - vector2
    assert(result == Vector(-1, -1, -2))
  }

  test("Negating a vector") {
    val result = -Vector(1, 1, 1)
    assert(result == Vector(-1, -1, -1))
  }

  test("Multiplying a vector by a scalar") {
    val result = Vector(1, 1, 1) * 2
    assert(result == Vector(2, 2, 2))
  }

  test("Dividing a vector by a scalar") {
    val result = Vector(1, 1, 1) / 2
    assert(result == Vector(0.5f, 0.5f, 0.5f))
  }

  test("Vector magnitude (1,0,0)") {
    val result = Vector(1, 0, 0).magnitude()
    assert(result == 1)
  }

  test("Vector magnitude (0,1,0)") {
    val result = Vector(0, 1, 0).magnitude()
    assert(result == 1)
  }

  test("Vector magnitude (1,2,3)") {
    val result = Vector(1, 2, 3).magnitude()
    assert(result >= (Math.sqrt(14) - errorEpsilon) && result <= (Math.sqrt(14) + errorEpsilon))
  }

  test("Normalising (4,0,0)") {
    val result = Vector(4, 0, 0).normalized()
    assert(result == Vector(1, 0, 0))
  }

  test("Normalising (1,2,3)") {
    val result = Vector(1, 2, 3).normalized()
    assert(result == Vector(1 / Math.sqrt(14).toFloat, 2 / Math.sqrt(14).toFloat, 3 / Math.sqrt(14).toFloat))
  }

  test("Dot product of (1,2,3), (2,3,4)") {
    val result = Vector(1, 2, 3).dot(Vector(2, 3, 4))
    assert(result == 20)
  }

  test("Cross product of (1,2,3), (2,3,4)") {
    val vecA = Vector(1, 2, 3)
    val vecB = Vector(2, 3, 4)
    assert(vecA.cross(vecB) == Vector(-1, 2, -1))
    assert(vecB.cross(vecA) == Vector(1, -2, 1))
  }

  test("Colour data is stored") {
    val c = Colour(1.0F, -0.5F, 0.7F)
    assert(c.r == 1.0F)
    assert(c.g == -0.5F)
    assert(c.b == 0.7F)
  }

  test("Adding colours") {
    val c1 = Colour(0.5F, -0.2F, 0.7F)
    val c2 = Colour(0.1F, 0.2F, 0.3F)

    assert(c1 + c2 == Colour(0.6F, 0F, 1.0F))
  }
  test("Subtracting colours") {
    val c1 = Colour(0.5F, -0.2F, 0.8F)
    val c2 = Colour(0.1F, 0.2F, 0.3F)

    assert(c1 - c2 == Colour(0.4F, -0.4F, 0.5F))
  }
  test("Multiplying a colour by a scalar") {
    val c1 = Colour(0.5F, -0.2F, 0.7F)

    assert(c1 * 2.0F == Colour(1F, -0.4F, 1.4F))
  }
  test("Multiplying colours") {
    val c1 = Colour(1F, 0.2F, 0.4F)
    val c2 = Colour(0.9F, 1F, 0.1F)

    assert(c1 * c2 == Colour(0.9F, 0.2F, 0.04F))
  }

  test("Canvas serialise") {
    val c = Canvas(5, 3)
    val c1 = Colour(1.5f, 0, 0)
    val c2 = Colour(0, 0.5f, 0)
    val c3 = Colour(-0.5f, 0, 1)
    c.writePixel(0, 0, c1)
    c.writePixel(2, 1, c2)
    c.writePixel(4, 2, c3)
    val a = c.serialize
    val r = 0
  }

  test("Splitting long lines into PPM files") {
    val c = Canvas(10, 2)
    val c1 = Colour(1f, 0.8f, 0.6f)
    for (y <- 0 to 9) {
      for (x <- 0 to 1) {
        c.writePixel(y, x, c1)
      }
    }
    val a = c.serialize
    val data = a.drop(12)
    val expected =
      """255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
        |153 255 204 153 255 204 153 255 204 153 255 204 153
        |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
        |153 255 204 153 255 204 153 255 204 153 255 204 153
        |""".stripMargin
    assert(data == expected)
  }

  test("Matrix construction and inspection") {
    val m1 = Matrix(Array(
      Array(1f   , 2f   , 3f   , 4f   ),
      Array(5.5f , 6.5f , 7.5f , 8.5f ),
      Array(9f   , 10f  , 11f  , 12f  ),
      Array(13.5f, 14.5f, 15.5f, 16.5f),
    ))
    assert(m1(0,0) == 1f)
    assert(m1(0,3) == 4f)
    assert(m1(1,0) == 5.5f)
    assert(m1(1,2) == 7.5f)
    assert(m1(2,2) == 11f)
    assert(m1(3,0) == 13.5f)
    assert(m1(3,2) == 15.5f)
  }

  test("Matrix equality") {
    val m1 = Matrix(Array(
      Array(1f , 2f , 3f , 4f ),
      Array(5f , 6f , 7f , 8f ),
      Array(9f , 10f, 11f, 12f),
      Array(13f, 14f, 15f, 16f),
    ))
    val m2 = Matrix(Array(
      Array(1f , 2f , 3f , 4f ),
      Array(5f , 6f , 7f , 8f ),
      Array(9f , 10f, 11f, 12f),
      Array(13f, 14f, 15f, 16f),
    ))
    assert(m1 == m2)
  }

  test("Matrix inequality") {
    val m1 = Matrix(Array(
      Array(1f , 2f , 3f , 4f ),
      Array(5f , 6f , 7f , 8f ),
      Array(9f , 10f, 11f, 12f),
      Array(13f, 14f, 15f, 16f),
    ))
    val m2 = Matrix(Array(
      Array(1f , 2f , 3f , 4f ),
      Array(5f , 6f , 7f , 8f ),
      Array(9f , 10f, 11f, 12f),
      Array(4f , 3f , 2f , 1f ),
    ))
    assert(m1 != m2)
  }

  test("Matrix element assignment") {
    val m1 = Matrix(Array(
      Array(1f , 2f , 3f , 4f ),
      Array(5f , 6f , 7f , 8f ),
      Array(9f , 10f, 11f, 12f),
      Array(13f, 14f, 15f, 16f),
    ))
    val expected = Matrix(Array(
      Array(1f , 2f , 3f , 4f ),
      Array(5f , 3f , 7f , 8f ),
      Array(9f , 10f, 11f, 12f),
      Array(13f, 14f, 15f, 16f),
    ))
    val matrix = m1(1,1) = 3f
    assert(matrix == expected)
  }

  test("Matrix multiplication") {
    val m1 = Matrix(Array(
      Array(1f , 2f , 3f , 4f ),
      Array(5f , 6f , 7f , 8f ),
      Array(9f , 8f , 7f , 6f ),
      Array(5f , 4f , 3f , 2f ),
    ))
    val m2 = Matrix(Array(
      Array(-2f   , 1f  , 2f  , 3f ),
      Array(3f    , 2f  , 1f  , -1f),
      Array(4f    , 3f  , 6f  , 5f ),
      Array(1f    , 2f  , 7f  , 8f ),
    ))
    val expected = Matrix(Array(
      Array(20, 22, 50, 48),
      Array(44, 54, 114, 108),
      Array(40, 58, 110, 102),
      Array(16, 26, 46, 42),
    ))
    val matrix = m1 * m2
    assert(matrix == expected)
  }
  test("Different size matrix multiplication") {
    val m1 = Matrix(Array(
      Array(1f , 2f , 3f , 4f ),
      Array(5f , 6f , 7f , 8f ),
      Array(9f , 8f , 7f , 6f ),
      Array(5f , 4f , 3f , 2f ),
    ))
    val m2 = Matrix(Array(
      Array(1f),
      Array(2f),
      Array(1f),
      Array(1f),
    ))
    val expected = Matrix(Array(
      Array(12f),
      Array(32f),
      Array(38f),
      Array(18f),
    ))
    val matrix = m1 * m2
    assert(matrix == expected)
  }
  test("Identity matrix multiplication") {
    val Identity = Matrix(Array(
      Array(1f , 0f , 0f , 0f ),
      Array(0f , 1f , 0f , 0f ),
      Array(0f , 0f , 1f , 0f ),
      Array(0f , 0f , 0f , 1f ),
    ))
    val vec = Matrix(Array(
      Array(1f),
      Array(2f),
      Array(1f),
      Array(1f),
    ))
    val expected = Matrix(Array(
      Array(1f),
      Array(2f),
      Array(1f),
      Array(1f),
    ))
    val matrix = Identity * vec
    assert(matrix == expected)
  }
  test("Transposing a matrix") {
    val m1 = Matrix(Array(
      Array(0f , 9f , 3f , 0f ),
      Array(9f , 8f , 0f , 8f ),
      Array(1f , 8f , 5f , 3f ),
      Array(0f , 0f , 5f , 8f ),
    ))
    val expected = Matrix(Array(
      Array(0f , 9f , 1f , 0f ),
      Array(9f , 8f , 8f , 0f ),
      Array(3f , 0f , 5f , 5f ),
      Array(0f , 8f , 3f , 8f ),
    ))
    assert(m1.transpose() == expected)
  }
  test("Determinant of a 2 x 2 matrix") {
    val m1 = Matrix(Array(
      Array(1f , 5f),
      Array(-3f ,2f),
    ))
    assert(m1.determinant() == 17f)
  }
  test("Submatrix of a 3x3 matrix is a 2x2 matrix") {
    val m1 = Matrix(Array(
      Array( 1, 5,  0),
      Array(-3, 2,  7),
      Array( 0, 6, -3),
    ))
    val subM1 = Matrix(Array(
      Array(-3, 2),
      Array( 0, 6),
    ))

    assert(m1.submatrix(0, 2) == subM1)
  }
  test("Submatrix of a 4x4 matrix is a 3x3 matrix") {
    val m1 = Matrix(Array(
      Array(-6, 1, 1, 6),
      Array(-6, 5, 8, 6),
      Array(-1, 0, 0, 2),
      Array(-7, 1, -1, 1),
    ))
    val subM1 = Matrix(Array(
      Array(-6, 1, 6),
      Array(-6, 8, 6),
      Array(-7, -1, 1),
    ))

    assert(m1.submatrix(2, 1) == subM1)
  }
  test("Minor of a 3x3 matrix") {
    val m1 = Matrix(Array(
      Array(3, 5, 0),
      Array(2, -1, -7),
      Array(6, -1, 5),
    ))
    val B = m1.submatrix(1, 0)

    assert(B.determinant() == 25)
    assert(m1.minor(1, 0) == 25)
  }
  test("Cofactor of a 3x3 matrix") {
    val m1 = Matrix(Array(
      Array(3, 5, 0),
      Array(2, -1, -7),
      Array(6, -1, 5),
    ))

    assert(m1.minor(0, 0) == -12)
    assert(m1.cofactor(0, 0) == -12)
    assert(m1.minor(1, 0) == 25)
    assert(m1.cofactor(1, 0) == -25)
  }
  test("Determinant of a 3x3 matrix") {
    val m1 = Matrix(Array(
      Array(1, 2, 6),
      Array(-5, 8, -4),
      Array(2, 6, 4),
    ))

    assert(m1.cofactor(0, 0) == 56)
    assert(m1.cofactor(0, 1) == 12)
    assert(m1.cofactor(0, 2) == -46)
    assert(m1.determinant() == -196)
  }
  test("Determinant of a 4x4 matrix") {
    val m1 = Matrix(Array(
      Array(-2, -8,  3,  5),
      Array(-3,  1,  7,  3),
      Array( 1,  2, -9,  6),
      Array(-6,  7,  7, -9)
    ))

    assert(m1.cofactor(0, 0) == 690)
    assert(m1.cofactor(0, 1) == 447)
    assert(m1.cofactor(0, 2) == 210)
    assert(m1.cofactor(0, 3) == 51)
    assert(m1.determinant() == -4071)
  }
  test("Test invertible matrix for invertibility") {
    val m1 = Matrix(Array(
      Array(6,  4, 4,  4),
      Array(5,  5, 7,  6),
      Array(4, -9, 3, -7),
      Array(9,  1, 7, -6),
    ))

    assert(m1.determinant() == -2120)
    assert(m1.isInvertible)
  }
  test("Test non-invertible matrix for invertibility") {
    val m1 = Matrix(Array(
      Array(-4, 2, -2, -3),
      Array(9,  6,  2, 6),
      Array(0, -5,  1, -5),
      Array(0,  0,  0, 0),
    ))

    assert(m1.determinant() == 0)
    assert(!m1.isInvertible)
  }
  test("Calculate the inverse of a matrix") {
    val m1 = Matrix(Array(
      Array(-5, 2, 6, -8),
      Array(1, -5, 1, 8),
      Array(7,  7, -6, -7),
      Array(1, -3, 7, 4),
    ))

    val inverse = m1.inverse()
    val formattedDecimalInverse = Approx.truncateMatrixDecimals(inverse)

    val expected = Matrix(Array(
      Array(0.21805f, 0.45113f, 0.24060f, -0.04511f),
      Array(-0.80827f, -1.45677f, -0.44361f, 0.52068f),
      Array(-0.07895f, -0.22368f, -0.05263f, 0.19737f),
      Array(-0.52256f, -0.81391f, -0.30075f, 0.30639f),
    ))

    assert(m1.determinant() == 532)
    assert(m1.cofactor(2,3) == -160)
    assert(inverse(3,2) == (-160/532.0).toFloat)
    assert(m1.cofactor(3,2) == 105)
    assert(inverse(2,3) == (105/532.0).toFloat)
    assert(formattedDecimalInverse == expected)
  }
  test("Calculate the inverse of another matrix") {
    val A = Matrix(Array(
      Array( 3, -9,  7,  3),
      Array( 3, -8,  2, -9),
      Array(-4,  4,  4,  1),
      Array(-6,  5, -1,  1),
    ))

    val B = Matrix(Array(
      Array(8,  2, 2, 2),
      Array(3, -1, 7, 0),
      Array(7,  0, 5, 4),
      Array(6, -2, 0, 5),
    ))

    val C = A * B
    val D = C * B.inverse()
    val dApprox = Approx.truncateMatrixDecimals(D)
    assert(A == dApprox)
  }
  test("Multiplying by a translation matrix") {
    val translateMatrix = translation(5, -3, 2)
    val p = Point(-3, 4, 5)

    val transformed = translateMatrix * p
    assert(transformed == Point(2, 1, 7))
  }
  test("Multiplying by the inverse a translation matrix") {
    val translateMatrix = translation(5, -3, 2)
    val p = Point(-3, 4, 5)

    val transformed = translateMatrix.inverse() * p
    assert(transformed == Point(-8, 7, 3))
  }
  test("Translation does not affect vectors") {
    val translateMatrix = translation(5, -3, 2)
    val v = Vector(-3, 4, 5)

    val transformed = translateMatrix * v
    assert(transformed == Vector(-3, 4, 5))
  }
  test("A scaling matrix applied to a point") {
    val scalingMatrix = scaling(2, 3, 4)
    val v = Point(-4, 6, 8)

    val transformed = scalingMatrix * v
    assert(transformed == Point(-8, 18, 32))
  }
  test("A scaling matrix applied to a vector") {
    val scalingMatrix = scaling(2, 3, 4).inverse()
    val v = Vector(-4, 6, 8)

    val transformed = scalingMatrix * v
    assert(transformed == Vector(-2, 2, 2))
  }
  test("Multiplying by the inverse of a scaling matrix") {
    val scalingMatrix = scaling(2, 3, 4).inverse()
    val v = Vector(-4, 6, 8)

    val transformed = scalingMatrix * v
    assert(transformed == Vector(-2, 2, 2))
  }
  test("Reflection is scaling by a negative value") {
    val scalingMatrix = scaling(-1, 1, 1)
    val p = Point(2,3,4)

    val transformed = scalingMatrix * p
    assert(transformed == Point(-2, 3, 4))
  }
  test("Rotating a point around the x-axis") {
    val halfQuarterMatrix = rotateX((Math.PI / 4).toFloat)
    val fullQuarterMatrix = rotateX((Math.PI / 2).toFloat)
    val p = Point(0,1,0)

    val half = halfQuarterMatrix * p
    val full = truncateDecimals(fullQuarterMatrix * p)

    assert(half == Point(0, (sqrt(2)/2).toFloat, (sqrt(2)/2).toFloat))
    assert(full == Point(0, 0, 1))
  }

  test("The inverse of an x-rotation rotates in the opposite direction") {
    val halfQuarterMatrix = rotateX((Math.PI / 4).toFloat)
    val inverse = halfQuarterMatrix.inverse()
    val p = Point(0,1,0)

    val expected = truncateDecimals(Point(0, (sqrt(2)/2).toFloat, -(sqrt(2)/2).toFloat))

    assert(truncateDecimals(inverse * p) == expected)
  }

  test("Rotating a point around the y-axis") {
    val halfQuarterMatrix = rotateY((Math.PI / 4).toFloat)
    val fullQuarterMatrix = rotateY((Math.PI / 2).toFloat)
    val p = Point(0,0,1)

    val half = halfQuarterMatrix * p
    val full = truncateDecimals(fullQuarterMatrix * p)

    assert(half == Point((sqrt(2)/2).toFloat, 0, (sqrt(2)/2).toFloat))
    assert(full == Point(1, 0, 0))
  }

  test("Rotating a point around the z-axis") {
    val halfQuarterMatrix = rotateZ((Math.PI / 4).toFloat)
    val fullQuarterMatrix = rotateZ((Math.PI / 2).toFloat)
    val p = Point(0,1,0)

    val half = halfQuarterMatrix * p
    val full = truncateDecimals(fullQuarterMatrix * p)

    assert(half == Point(-(sqrt(2)/2).toFloat, (sqrt(2)/2).toFloat, 0))
    assert(full == Point(-1, 0, 0))
  }

  test("A shearing transformation moves x in proportion to y") {
    val shearingMatrix = shearing(1,0,0,0,0,0)
    val p = Point(2,3,4)

    assert(shearingMatrix * p == Point(5, 3, 4))
  }
  test("A shearing transformation moves x in proportion to z") {
    val shearingMatrix = shearing(0,1,0,0,0,0)
    val p = Point(2,3,4)

    assert(shearingMatrix * p == Point(6, 3, 4))
  }
  test("A shearing transformation moves y in proportion to x") {
    val shearingMatrix = shearing(0,0,1,0,0,0)
    val p = Point(2,3,4)

    assert(shearingMatrix * p == Point(2,5,4))
  }
  test("A shearing transformation moves y in proportion to z") {
    val shearingMatrix = shearing(0,0,0,1,0,0)
    val p = Point(2,3,4)

    assert(shearingMatrix * p == Point(2,7,4))
  }
  test("A shearing transformation moves z in proportion to x") {
    val shearingMatrix = shearing(0,0,0,0,1,0)
    val p = Point(2,3,4)

    assert(shearingMatrix * p == Point(2,3,6))
  }
  test("A shearing transformation moves z in proportion to y") {
    val shearingMatrix = shearing(0,0,0,0,0,1)
    val p = Point(2,3,4)

    assert(shearingMatrix * p == Point(2,3,7))
  }
  test("Individual transformations are applied in sequence") {
    val p = Point(1,0,1)
    val rotationMatrix = rotateX((Pi / 2).toFloat)
    val scalingMatrix = scaling(5, 5, 5)
    val translationMatrix = translation(10, 5, 7)

    val p2 = truncateDecimals(rotationMatrix * p)
    val p3 = truncateDecimals(scalingMatrix * p2)
    val p4 = truncateDecimals(translationMatrix * p3)

    assert(p2 == Point(1, -1, 0))
    assert(p3 == Point(5, -5, 0))
    assert(p4 == Point(15, 0, 7))
  }

  test("Chained transformations must be applied in the reverse order") {
    val p = Point(1,0,1)
    val rotationMatrix = rotateX((Pi / 2).toFloat)
    val scalingMatrix = scaling(5, 5, 5)
    val translationMatrix = translation(10, 5, 7)

    val transformationMatrix = translationMatrix * scalingMatrix * rotationMatrix
    val result = transformationMatrix * p

    assert(result == Point(15, 0, 7))
  }

  test("Creating and querying a ray") {
    val origin = Point(1,2,3)
    val direction = Vector(4,5,6)
    val ray = Ray(origin, direction)

    assert(ray.origin == origin)
    assert(ray.direction == direction)
  }

  test("Computing a point from a distance") {
    val ray = Ray(Point(2,3,4), Vector(1,0,0))

    assert(ray.position(0) == Point(2,3,4))
    assert(ray.position(1) == Point(3,3,4))
    assert(ray.position(-1) == Point(1,3,4))
    assert(ray.position(2.5f) == Point(4.5f, 3, 4))
  }

  test("A ray intersects a sphere at two points") {
    val ray = Ray(Point(0,0,-5), Vector(0,0,1))
    val sphere = Sphere()
    val xs = ray.intersect(sphere)

    assert(xs.length == 2)
    assert(xs(0).t == 4)
    assert(xs(1).t == 6)
  }
  test("A ray intersects a sphere at a tangent") {
    val ray = Ray(Point(0,1,-5), Vector(0,0,1))
    val sphere = Sphere()
    val xs = ray.intersect(sphere)

    assert(xs.length == 2)
    assert(xs(0).t == 5)
    assert(xs(1).t == 5)
  }
  test("A ray misses a sphere") {
    val ray = Ray(Point(0,2,-5), Vector(0,0,1))
    val sphere = Sphere()
    val xs = ray.intersect(sphere)

    assert(xs.length == 0)
  }
  test("A ray originates inside a sphere") {
    val ray = Ray(Point(0,0,0), Vector(0,0,1))
    val sphere = Sphere()
    val xs = ray.intersect(sphere)

    assert(xs.length == 2)
    assert(xs(0).t == -1)
    assert(xs(1).t == 1)
  }
  test("A sphere is behind a ray") {
    val ray = Ray(Point(0,0,5), Vector(0,0,1))
    val sphere = Sphere()
    val xs = ray.intersect(sphere)

    assert(xs.length == 2)
    assert(xs(0).t == -6)
    assert(xs(1).t == -4)
  }
  test("An intersection encapsulates t and object") {
    val sphere = Sphere()
    val i = Intersection(3.5f, sphere)

    assert(i.t == 3.5)
    assert(i.obj == sphere)
  }
  test("Aggregating intersections") {
    val sphere = Sphere()
    val i1 = Intersection(1, sphere)
    val i2 = Intersection(2, sphere)

    val xs = intersections(i1, i2)

    assert(xs.length == 2)
    assert(xs(0).obj == sphere)
    assert(xs(1).obj == sphere)
  }
  test("Intersect sets the object on the intersection") {
    val r = Ray(Point(0,0,-5), Vector(0,0,1))
    val s = Sphere()
    val xs = r.intersect(s)

    assert(xs.length == 2)
    assert(xs(0).obj == s)
    assert(xs(1).obj == s)
  }
  test("The hit, when all intersections have a positive t") {
    val s = Sphere()
    val i1 = Intersection(1, s)
    val i2 = Intersection(2, s)
    val xs = intersections(i2, i1)

    val i = hit(xs)
    assert(i(0) == i1)
  }
  test("The hit, when some intersections have a negative t") {
    val s = Sphere()
    val i1 = Intersection(-1, s)
    val i2 = Intersection(1, s)
    val xs = intersections(i2, i1)

    val i = hit(xs)
    assert(i(0) == i2)
  }
  test("The hit, when all intersections have a negative t") {
    val s = Sphere()
    val i1 = Intersection(-2, s)
    val i2 = Intersection(-1, s)
    val xs = intersections(i2, i1)

    val i = hit(xs)
    assert(i.length == 0)
  }
  test("The hit is always the lowest nonnegative intersection") {
    val s = Sphere()
    val i1 = Intersection(5, s)
    val i2 = Intersection(7, s)
    val i3 = Intersection(-3, s)
    val i4 = Intersection(2, s)
    val xs = intersections(i1, i2, i3, i4)

    val i = hit(xs)
    assert(i(0) == i4)
  }
}

import scala.math.{cos, sin}

object Transformations {
  def translation(i: Float, j: Float, k: Float): Matrix =
    Matrix(Array(
      Array(1, 0, 0, i),
      Array(0, 1, 0, j),
      Array(0, 0, 1, k),
      Array(0, 0, 0, 1),
    ))

  def scaling(i: Float, j: Float, k: Float): Matrix =
    Matrix(Array(
      Array(i, 0, 0, 0),
      Array(0, j, 0, 0),
      Array(0, 0, k, 0),
      Array(0, 0, 0, 1),
    ))

  def rotateX(rad: Float): Matrix =
    Matrix(Array(
      Array(1, 0,                0,                 0),
      Array(0, cos(rad).toFloat, -sin(rad).toFloat, 0),
      Array(0, sin(rad).toFloat, cos(rad).toFloat,  0),
      Array(0, 0,                0,                 1),
    ))
  def rotateY(rad: Float): Matrix =
    Matrix(Array(
      Array(cos(rad).toFloat,  0, sin(rad).toFloat, 0),
      Array(0,                 1, 0,                0),
      Array(-sin(rad).toFloat, 0, cos(rad).toFloat, 0),
      Array(0,                 0, 0,                1),
    ))
  def rotateZ(rad: Float): Matrix =
    Matrix(Array(
      Array(cos(rad).toFloat, -sin(rad).toFloat, 0, 0),
      Array(sin(rad).toFloat, cos(rad).toFloat,  0, 0),
      Array(0,                0,                 1, 0),
      Array(0,                0,                 0, 1),
    ))
  def shearing(Xy: Float, Xz: Float, Yx: Float, Yz: Float, Zx: Float, Zy: Float): Matrix =
    Matrix(Array(
      Array(1,  Xy, Xz, 0),
      Array(Yx, 1,  Yz, 0),
      Array(Zx, Zy, 1,  0),
      Array(0,  0,  0,  1),
    ))
}
import scala.util.control.Breaks.{break, breakable}

case class Matrix(data: Array[Array[Float]]) {
  val w: Int = data(0).length
  val h: Int = data.length

  def apply(i: Int, j: Int): Float = data(i)(j) // Indexing

  def update(i: Int, j: Int, value: Float) = { // Assignment
    data(i)(j) = value
    Matrix(data)
  }

  private def checkAllElementsMatch(other: Matrix): Boolean = {
    var allMatch = true
    // Check data
    breakable {
      for (i <- data.indices) {
        for (j <- data(0).indices) {
          if (!allMatch) {
            break
          }
          allMatch = data(i)(j) == other.data(i)(j)
        }
      }
    }
    if (allMatch) true else false
  }

  def ==(other: Matrix): Boolean = {
    // Check sizes
    val widthMatch = data(0).length == other.data(0).length
    val heightMatch = data.length == other.data.length

    if (widthMatch && heightMatch)
      checkAllElementsMatch(other)
    else
      false
  }

  def !=(other: Matrix): Boolean = {
    // Check sizes
    val widthMatch = data(0).length == other.data(0).length
    val heightMatch = data.length == other.data.length

    if (!widthMatch || !heightMatch)
      true
    else
      !checkAllElementsMatch(other)
  }

  def *(other: Matrix): Matrix = {
    val widthUs = data(0).length
    val heightThem = other.data.length

    val isMultiplyable = widthUs == heightThem
    if (isMultiplyable) {
      val newMatrix = Matrix(
        Array.fill(data.length)(Array.fill(other.data(0).length)(0f))
      )

      for (x <- newMatrix.data(0).indices) {
        for (y <- newMatrix.data.indices) {
          var sum = 0f
          // Full width x height
          for (n <- Range.inclusive(0, w - 1)) {
            val cellX = data(y)(n)
            val cellY = other.data(n)(x)
            sum += cellX * cellY
          }
          newMatrix.data(y)(x) = sum
        }
      }
      newMatrix
    } else {
      throw new RuntimeException(s"Bad matrix dimensions. ${data.length}x${widthUs} and ${heightThem}x${other.data(0).length}")
    }
  }

  def *(other: Vector): Vector = {
    val widthUs = data(0).length
    val heightThem = 4

    val isMultiplyable = widthUs == heightThem
    if (isMultiplyable) {
      val newX = data(0)(0) * other.x + data(0)(1) * other.y + data(0)(2) * other.z + data(0)(3) * other.w
      val newY = data(1)(0) * other.x + data(1)(1) * other.y + data(1)(2) * other.z + data(1)(3) * other.w
      val newZ = data(2)(0) * other.x + data(2)(1) * other.y + data(2)(2) * other.z + data(2)(3) * other.w
      Vector(newX, newY, newZ)
    } else {
      throw new RuntimeException(s"Bad matrix dimensions. ${data.length}x${widthUs} and Vec4")
    }
  }

  def transpose(): Matrix = {
    val newMatrix = Matrix(
      Array.fill(data(0).length)(Array.fill(data.length)(0f))
    )
    for (y <- Range.inclusive(0, h - 1)) {
      for (x <- Range.inclusive(0, w - 1)) {
        newMatrix(x, y) = data(y)(x)
      }
    }
    newMatrix
  }

  def determinant(): Float = {
    data(0)(0) * data(1)(1) - data(0)(1) * data(1)(0)
  }

  def submatrix(r: Int, c: Int): Matrix = {
    val newMatrixWithoutRow = Matrix(Array.fill(data(0).length - 1)(Array.fill(data.length)(0f)))
    for (y <- Range.inclusive(0, h - 1)) {
      for (x <- Range.inclusive(0, w - 1)) {
        if (y < r) {
          newMatrixWithoutRow(y, x) = data(y)(x)
        } else if (y > r) {
          newMatrixWithoutRow(y - 1, x) = data(y)(x)
        }
      }
    }
    val newMatrixWithoutCol = Matrix(
      Array.fill(newMatrixWithoutRow.h)(Array.fill(newMatrixWithoutRow.w - 1)(0f))
    )
    for (y <- Range.inclusive(0, newMatrixWithoutRow.h - 1)) {
      for (x <- Range.inclusive(0, newMatrixWithoutRow.w - 1)) {
        if (x < c) {
          newMatrixWithoutCol(y, x) = newMatrixWithoutRow.data(y)(x)
        } else if (x > c) {
          newMatrixWithoutCol(y, x - 1) = newMatrixWithoutRow.data(y)(x)
        }
      }
    }
    newMatrixWithoutCol
  }
}

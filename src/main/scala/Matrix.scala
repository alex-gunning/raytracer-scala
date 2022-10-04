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
        for (j <- data.indices) {
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
      var newMatrix = Matrix(
        Array.fill(data.length)(Array.fill(other.data(0).length)(0f))
      )
      val newW = newMatrix.w

      for(x <- newMatrix.data(0).indices) {
        for(y <- newMatrix.data.indices) {
          var sum = 0f
          // Full width x height
          for(n <- Range.inclusive(0, newW - 1)) {
            val cellX = data(x)(n)
            val cellY = other.data(n)(y)
            sum += cellX * cellY
          }
          newMatrix(x, y) = sum
        }
      }
      newMatrix
    } else {
      throw new RuntimeException(s"Bad matrix dimensions. ${data.length}x${widthUs} and ${heightThem}x${other.data(0).length}")
    }
  }
}

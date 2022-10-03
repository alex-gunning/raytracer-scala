import Approx.errorEpsilon

import scala.util.control.Breaks.{break, breakable}

object Approx {
  val errorEpsilon = 0.00001

  def clamp(number: Int, min: Int = 0, max: Int = 255) = Math.max(min, Math.min(max, number))
}


case class Colour(r: Float, g: Float, b: Float) {
  def +(other: Colour) = Colour(r + other.r, g + other.g, b + other.b)

  def -(other: Colour) = Colour(r - other.r, g - other.g, b - other.b)

  def *(scalar: Float) = Colour(r * scalar, g * scalar, b * scalar)

  def *(other: Colour) = Colour(r * other.r, g * other.g, b * other.b)

  def ==(other: Colour) = {
    val redEqual = (r - other.r) > -errorEpsilon && (r - other.r) < errorEpsilon
    val blueEqual = (b - other.b) > -errorEpsilon && (b - other.b) < errorEpsilon
    val greenEqual = (g - other.g) > -errorEpsilon && (g - other.g) < errorEpsilon
    redEqual && blueEqual && greenEqual
  }
}

case class Canvas(w: Int, h: Int) {
  var canvasArray: Array[Array[Colour]] = Array.fill(h) {
    Array.fill(w) {
      Colour(0, 0, 0)
    }
  }

  def writePixel(x: Int, y: Int, colour: Colour) = {
    if (x < w && x >= 0 && y < h && y >= 0)
      canvasArray(y)(x) = colour
  }

  def pixelAt(x: Int, y: Int, colour: Colour): Colour = canvasArray(y)(x)

  def serialize: String = {
    def toPPMColour(colourIdx: Float) = Approx.clamp((colourIdx * 255).toInt)

    val colourData = canvasArray
      .map(y => y.map(c => s"${toPPMColour(c.r)} ${toPPMColour(c.g)} ${toPPMColour(c.b)}")
        .reduce((a, b) => a.concat(" ").concat(b)))
      .map(line => {
        // Split into lines of maximum 70 characters long. Start at char 70 and move back until a space, use it to split.
        if (line.length > 70) {
          var currSplitIdx = 70
          var charAtLine = line.charAt(currSplitIdx)
          while (charAtLine != ' ') {
            currSplitIdx = currSplitIdx - 1
            charAtLine = line.charAt(currSplitIdx)
          }
          val splitLine = line.substring(0, currSplitIdx).concat("\n").concat(line.substring(currSplitIdx + 1))
          splitLine
        } else {
          line
        }
      })
      .reduce((a, b) => a.concat("\n").concat(b))


    // Add newline to the end
    //    colourData = colourData.concat("\n")

    s"""|P3
        |$w $h
        |255
        |${colourData}
        |""".stripMargin
  }
}

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import colour._

import scala.math.pow

object allRGB {
  val average = false
  val palette:ColourPalette = new ColourPalette(12)

  /* This guarantees that the resulting image will either be square, or
   * have a 2:1 height:width ratio.
   */
  val height:Int = pow(2, palette.bits / 2).toInt
  val width:Int = palette.colours.length / height

  case class Coordinate(x: Int, y: Int) {
    def equals(other: Coordinate): Boolean = {
      (other.x == x) && (other.y == y)
    }

    lazy val neighbours: List[Coordinate] = {
      val rng = List(-1, 0, 1)
      (for {
        dy <- rng
        y1 = y + dy
        if 0 <= y1 && y1 <= height
      } yield {
        for {
          dx <- rng
          x1 = x + dx
          if 0 <= x1 && x1 <= width
        } yield {
          Coordinate(x1, y1)
        }
      }).flatten
    }


    def calcDiff(pixels: Map[Coordinate, Option[Colour]], colour: Colour): Int = {
      val neighbourColours: List[Option[Colour]] = for {
        neighbour <- neighbours
      } yield {
        pixels.get(neighbour) match {
          case None =>
            None
          case Some(colourOption) =>
            colourOption
        }
      }

      val diffs: List[Option[Int]] = for {
        neighbourColour <- neighbourColours
        if ! (neighbourColour equals None)
      } yield colour.difference(neighbourColour)

      def sumOption(left: Int, rightOption: Option[Int]): Int = {
        rightOption match {
          case None =>
            left// + (2 << Colour.bitDepth)
          case Some(right) =>
            left + right
        }
      }

      if (average) {
        diffs.foldLeft(0)(sumOption) / diffs.size
      } else {
        diffs.min.get
      }
    }
  }

  def isColour(colourOption: Option[Colour]): Boolean = {
    colourOption match {
      case None => false
      case Some(colour) => true
    }
  }

  def getAvailable(pixels: Map[Coordinate, Option[Colour]]): List[Coordinate] = {
    var ret = Set[Coordinate]()
    for {
      (coordinate, colourOption) <- pixels
      if ! isColour(colourOption)
    } for {
        neighbour <- coordinate.neighbours
        if ! ret.contains(neighbour)
        if isColour(pixels.get(neighbour) match {
          case None => None
          case Some(neighbourColourOption) =>
            neighbourColourOption match {
              case None => None
              case(neighbourColour) =>
                neighbourColourOption
            }
        })
      } ret += coordinate
    ret.toList
  }

  def savePNG(pixels: Map[Coordinate, Option[Colour]]): Unit = {
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val canvas = img.createGraphics()

    for {
      (coordinate, colourOption) <- pixels
    } {
      colourOption match {
        case None =>
        case Some(colour) =>
          canvas.setColor(colour.awt)
          canvas.fillRect(coordinate.x, coordinate.y, 1, 1)
      }
    }
    canvas.dispose()


    ImageIO.write(img, "png", new java.io.File("allRGB.png"))
  }

  def closest(pixels: Map[Coordinate, Option[Colour]],
              colour: Colour,
              available: List[Coordinate]): Coordinate = {

    var closestTemp = available.head
    var closestDiff = closestTemp.calcDiff(pixels, colour)

    for {
        c <- available
        d = c.calcDiff(pixels, colour)
        if d < closestDiff
      } {
        closestTemp = c
        closestDiff = d
      }

    closestTemp
  }

  def main (args: Array[String]): Unit = {

    var pixels: Map[Coordinate, Option[Colour]] = (
      for { x <- List.range(0, width)
            y <- List.range(0, height) } yield new Coordinate(x, y) -> None
      ).toMap

    assert(pixels.size == palette.colours.length, "Number of pixels must match the number of colours")

    for {
      colour <- palette.colours
    }{

      val available = getAvailable(pixels)
      val coordinate: Coordinate = if (available.length == 0) {
       Coordinate(width / 2, height / 2)
      } else {
        closest(pixels, colour, available)
      }

      assert(pixels(coordinate) equals None, s"Pixel must be blank: $coordinate -> ${pixels(coordinate)}")

      pixels += (coordinate -> Option(colour))
    }

    savePNG(pixels)

  }
}

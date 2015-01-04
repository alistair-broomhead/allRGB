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
          case Some(colourOption) =>
            colourOption
          case None =>
            None
        }
      }

      val diffs: List[Option[Int]] = for {
        neighbour <- neighbourColours
      } yield colour.difference(neighbour)

      def sumOption(left: Int, rightOption: Option[Int]): Int = {
        rightOption match {
          case Some(right) =>
            left + right
          case None =>
            left
        }
      }

      if (average) {
        diffs.foldLeft(0)(sumOption) / diffs.size
      } else {
        diffs.min.getOrElse(0)
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

  def main (args: Array[String]): Unit = {

    var pixels: Map[Coordinate, Option[Colour]] = (
      for { x <- List.range(0, width)
            y <- List.range(0, height) } yield new Coordinate(x, y) -> None
      ).toMap
  }
}

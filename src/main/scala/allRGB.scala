import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import allRBG.colour.Colour
import allRBG.constants.Constants
import allRBG.drawing.Coordinate

object allRGB {

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

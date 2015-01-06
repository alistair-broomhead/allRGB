import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import allRBG.colour.Colour
import allRBG.constants.Constants
import allRBG.drawing.Coordinate

object allRGB {

  def closest(pixels: Map[Coordinate, Option[Colour]],
              colour: Colour,
              available: List[Coordinate]): Coordinate = {

    if (available.size == 1){
      available.head
    } else {
      var closestTemp = available.head
      var closestDiff = closestTemp.calcDiff(closestTemp.getNeighbourColours(pixels), colour)

      for {
        c <- available
        d = c.calcDiff(c.getNeighbourColours(pixels), colour)
        if d < closestDiff
      } {
        closestTemp = c
        closestDiff = d
      }

      closestTemp
    }
  }

  def main (args: Array[String]): Unit = {

    var pixels: Map[Coordinate, Option[Colour]] = (
      for { x <- List.range(0, Constants.imageWidth)
            y <- List.range(0, Constants.imageHeight) } yield new Coordinate(x, y) -> None
      ).toMap

    var available = Set[Coordinate](
      Coordinate(Constants.imageWidth / 2, Constants.imageHeight / 2)
    )

    assert(pixels.size == Constants.palette.colours.length, "Number of pixels must match the number of colours")

    val percentagePoint:Double = pixels.size.toDouble / 1000.0
    var nextPercentagePoint:Double = 0

    val img = new BufferedImage(Constants.imageWidth, Constants.imageHeight, BufferedImage.TYPE_INT_RGB)
    val canvas: Graphics2D = img.createGraphics()

    println("Generating image (Each '.' below is 0.1% progress)")

    var iterations: Int = 0
    for {
      colour <- Constants.palette.colours
    }{
      val coordinate = closest(pixels, colour, available.toList)

      assert(pixels(coordinate) equals None, s"Pixel must be blank: $coordinate -> ${pixels(coordinate)}")

      // This is the bit where we have all the nasty mutable state
      iterations += 1
      pixels += (coordinate -> Option(colour))

      for {
        neighbour <- coordinate.neighbours
        if ! Colour.isColourOption(pixels.getOrElse(neighbour, None))
      } available += neighbour

      available -= coordinate
      // Phew it's over!

      canvas.setColor(colour.awt)
      canvas.fillRect(coordinate.x, coordinate.y, 1, 1)

      // Side effects live here
      if (nextPercentagePoint <= iterations){

        while (nextPercentagePoint <= iterations) nextPercentagePoint += percentagePoint

        print(".")
      }
    }

    print(".\nComplete!")
    ImageIO.write(img, "png", new java.io.File("allRGB.png"))

    canvas.dispose()

  }
}

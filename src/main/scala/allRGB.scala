import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.imageio.stream.FileImageOutputStream

import allRBG.colour.Colour
import allRBG.constants.Constants
import allRBG.drawing.Coordinate

import scala.collection.mutable

object allRGB {

  def closest(pixels: mutable.Map[Coordinate, Colour],
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

    println("Initialising...")
    val imageSize = Constants.imageWidth * Constants.imageHeight

    val pixels: mutable.Map[Coordinate, Colour] = mutable.Map()
    val available = mutable.Set[Coordinate](
      Coordinate(Constants.imageWidth / 2, Constants.imageHeight / 2)
    )

    val percentagePoint:Double = imageSize / 1000.0
    var nextPercentagePoint:Double = 0

    val img = new BufferedImage(Constants.imageWidth, Constants.imageHeight, BufferedImage.TYPE_INT_RGB)

    val output = new FileImageOutputStream(new java.io.File(s"allRGB.gif"))
    val writer = new GifSequenceWriter(output, 1, 10, false)

    println("Generating image (Each '.' below is 0.1% progress)")

    var iterations: Int = 0
    for {
      colour <- Constants.palette.colours
    }{
      val coordinate = closest(pixels, colour, available.toList)

      val existingColour = pixels.getOrElse(coordinate, None)
      assert(existingColour.equals(None), s"Pixel must be blank: $coordinate -> $existingColour")

      // This is the bit where we have all the nasty mutable state
      iterations += 1
      pixels.update(coordinate, colour)

      for {
        neighbour <- coordinate.neighbours
        if pixels.getOrElse(neighbour, None).equals(None)
      } available.add(neighbour)

      available.remove(coordinate)
      // Phew it's over!

      img.setRGB(coordinate.x, coordinate.y, colour.awt.getRGB)

      // Side effects live here
      if (nextPercentagePoint <= iterations){

        while (nextPercentagePoint <= iterations){
          print(".")
          nextPercentagePoint += percentagePoint
        }

        try {
          writer.writeToSequence(img)
        } catch {
          case e: Exception =>
            ImageIO.write(img, "png", new java.io.File(s"allRGB_$iterations.png"))
        }
      }
    }

    assert(iterations equals imageSize, s"$iterations equals $imageSize")
    print(".\nComplete!")
    ImageIO.write(img, "png", new java.io.File("allRGB.png"))

    writer.close()
    output.close()

  }
}

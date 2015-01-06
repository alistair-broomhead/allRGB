package allRBG.constants

import allRBG.colour.ColourPalette
import scala.math.pow

object Constants {

  val average = false
  val palette = new ColourPalette(18)

  /* This guarantees that the resulting image will either be square, or
   * have a 2:1 height:width ratio.
   */
  val imageHeight = pow(2, palette.bits / 2).toInt
  val imageWidth = palette.colours.length / imageHeight
}

package allRBG.constants

import allRBG.colour.ColourPalette

object Constants {

  val average = false
  val random = false
  val palette = new ColourPalette(24)

  /* This guarantees that the resulting image will either be square, or
   * have a 2:1 height:width ratio.
   */
  private val imageHeightBits = palette.bits / 2
  private val imageWidthBits = palette.bits - imageHeightBits

  val imageHeight = 1 << imageHeightBits
  val imageWidth = 1 << imageWidthBits
}

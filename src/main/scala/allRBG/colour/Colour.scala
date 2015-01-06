package allRBG.colour

import java.awt.{Color => awtColour}

object Colour {
  val bitDepth:Int = 24
  val bitsPerChannel: Int = bitDepth / 3
  val coloursPerChannel:Int = 1 << bitsPerChannel

  def isColourOption(colourOption: Option[Colour]): Boolean = {
    colourOption match {
      case None => false
      case Some(colour) => true
    }
  }
}

class Colour(val red: Int, val green: Int, val blue: Int){

  assert(0 <= red && red < 256, s"Red not in range [0, 256): $red")
  assert(0 <= green && green < 256, s"Green not in range [0, 256): $green")
  assert(0 <= blue && blue < 256, s"Blue not in range [0, 256): $blue")

  lazy val awt: awtColour = new awtColour(red, green, blue)

  override def toString: String = s"Colour($red, $green, $blue)"

  def difference(other: Colour): Int = {
      val r = red - other.red
      val g = green - other.green
      val b = blue - other.blue
      r*r + g*g + b*b
  }
}
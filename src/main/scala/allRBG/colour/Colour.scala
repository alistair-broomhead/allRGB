package allRBG.colour

import java.awt.{Color => awtColour}

import scala.math.pow

object Colour {
  val bitDepth:Int = 24
  val bitsPerChannel: Int = bitDepth / 3
  val coloursPerChannel:Int = pow(2, bitsPerChannel).toInt

  def isColourOption(colourOption: Option[Colour]): Boolean = {
    colourOption match {
      case None => false
      case Some(colour) => true
    }
  }
}

class Colour(val red: Int, val green: Int, val blue: Int){

  val awt: awtColour = new awtColour(red, green, blue)

  override def toString: String = s"Colour($red, $green, $blue)"

  def difference(other: Colour): Int = {
      val r = red - other.red
      val g = green - other.green
      val b = blue - other.blue
      r*r + g*g + b*b
  }
}

package allRBG.colour

import java.awt.{Color => awtColour}

import scala.math.pow

object Colour {
  val bitDepth:Int = 24
  val bitsPerChannel: Int = bitDepth / 3
  val coloursPerChannel:Int = pow(2, bitsPerChannel).toInt
}

class Colour(val red: Int, val green: Int, val blue: Int){

  val awt: awtColour = new awtColour(red, green, blue)

  override def toString: String = s"Colour($red, $green, $blue)"

  def toInt: Int = {
    val gMult = Colour.coloursPerChannel
    val bMult = gMult*gMult
    red + (green*gMult) + (blue*bMult)
  }

  def difference(otherOption: Option[Colour]): Option[Int] =
    otherOption match {
      case None =>
        None

      case Some(other) =>
        val r = red - other.red
        val g = green - other.green
        val b = blue - other.blue
        Option(r*r + g*g + b*b)
    }
}

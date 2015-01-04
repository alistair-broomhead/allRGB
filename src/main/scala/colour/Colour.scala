package colour

import scala.math.pow

object Colour {
  val bitDepth:Int = 24
  val bitsPerChannel: Int = bitDepth / 3
  val coloursPerChannel:Int = pow(2, bitsPerChannel).toInt
}

class Colour(val red: Int, val green: Int, val blue: Int){

  override def toString: String = s"Colour($red, $green, $blue)"

  def toInt: Int = {
    val gMult = Colour.coloursPerChannel
    val bMult = gMult*gMult
    red + (green*gMult) + (blue*bMult)
  }

  def difference(otherOption: Option[Colour]): Option[Int] =
    otherOption match {
      case Some(other) =>
        Option(toInt - other.toInt)

      case None =>
        None
    }
}

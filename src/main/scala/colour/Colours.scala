package colour

import scala.math._
import scala.util.Random

object Colours {

  val maxBits:Int = Colour.bitDepth
  lazy val maxBitsPerChannel:Int = Colour.bitsPerChannel

}

class Colours(depth: Int = 15) {
  /*
   * The thing that determines the colour palette is the number of
   * bits available to represent each colour - the actual colours will
   * be spread evenly around a 24 bit colour space.
   */
  val bits = depth

  assert(bits % 3 == 0, "Colour depth must me a multiple of 3")
  assert(bits <= Colours.maxBits, "Maximum supported colour depth is 24 bit")

  lazy private val bitsPerChannel:Int = bits / 3
  lazy private val coloursPerChannel:Int = pow(2, bitsPerChannel).toInt
  lazy private val bitShift:Int = Colours.maxBitsPerChannel - bitsPerChannel

  // This will be the same for all palettes
  protected val paletteSize24Bit:Int = pow(2, 24).toInt

  // The number of colours in the palette
  lazy val numColours:Int = pow(2, bits).toInt

  private val channelValues: List[Int] = for {
    i <- List.range(0, coloursPerChannel)
  } yield i << bitShift

  // create actual colour palette
  private val rnd = new Random()
  lazy val colours:List[Colour] = (for {
    r <- channelValues
    g <- channelValues
    b <- channelValues
  } yield new Colour(r, g, b)
  ).sortBy((i) => rnd.nextInt())
}

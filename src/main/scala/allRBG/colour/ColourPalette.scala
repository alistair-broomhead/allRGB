package allRBG.colour

import allRBG.constants.Constants

import scala.util.Random

object ColourPalette {

  val maxBits:Int = Colour.bitDepth // maybe I should just call it quits and make this 21...
  lazy val maxBitsPerChannel:Int = Colour.bitsPerChannel
  lazy val maxColoursPerChannel: Int = 1 << maxBitsPerChannel

}

class ColourPalette(depth: Int = 15) {
  /*
   * The thing that determines the colour palette is the number of
   * bits available to represent each colour - the actual colours will
   * be spread evenly around a 24 bit colour space.
   */
  val bits = depth

  assert(bits % 3 == 0, "Colour depth must me a multiple of 3")
  assert(bits <= ColourPalette.maxBits, "Maximum supported colour depth is 24 bit")

  lazy private val bitsPerChannel:Int = bits / 3
  lazy private val coloursPerChannel:Int = 1 << bitsPerChannel
  lazy private val colourMult: Int = ColourPalette.maxColoursPerChannel / coloursPerChannel
  lazy private val bitShift: Int = ColourPalette.maxBitsPerChannel / bitsPerChannel

  // This will be the same for all palettes
  protected val paletteSize24Bit:Int = 1 << 24

  // The number of colours in the palette
  lazy val numColours:Int = 1 << bits

  // create actual colour palette
  private val rnd = new Random()

  private def shifted(v:Int):Int = {
    if (v == 0){
      0
    } else {
      v << bitShift-1
    }
  }

  private val getColours = new Iterator[Colour]{
    println("Generating palette...")
    var r = -1
    var g = 0
    var b = 0

    var going = true

    def hasNext = going

    def next(): Colour = {
      r += 1
      if (r == coloursPerChannel){
        r = 0
        g += 1
        if (g == coloursPerChannel){
          g = 0
          b += 1
        }
      }
      if (r == g && g == b && b == coloursPerChannel-1){
        going = false
        println("Generated palette!")
      }
      new Colour(shifted(r), shifted(g), shifted(b))
    }
  }

  val colours:Iterator[Colour] = {
    if (Constants.random){
      val c = getColours.toList
      println("Shuffling Palette...")
      val p = c.sortBy((i) => rnd.nextInt()).toIterator
      println("Shuffled Palette!")
      p
    } else {
      getColours
    }
  }
}

import colour._

import scala.math.pow




object allRGB {
  val average = false
  val colours:ColourPalette = new ColourPalette(6)

  /* This guarantees that the resulting image will either be square, or
   * have a 2:1 height:width ratio.
   */
  val height:Int = pow(2, colours.bits / 2).toInt
  val width:Int = colours.colours.length / height

  class Coordinate(var x: Int, var y: Int){
    def equals(other: Coordinate): Boolean = {
      (other.x == x) && (other.y == y)
    }
    def neighbours: List[Coordinate] = {
      for {
        dy <- List.range(
          if (y > 0) -1 else 0,
          if (y < height) 1 else 0)
      } yield {
        for {
          dx <- List.range(
            if (x > 0) -1 else 0,
            if (x < width) 1 else 0)
        } yield {
          new Coordinate(x + dx, y + dy)
        }
      }
    }.flatten
  }

  def calcDiff (neighbours: List[Option[Colour]], colour: Colour): Int = {
    val diffs:List[Option[Int]] = for {
      neighbour <- neighbours
    } yield colour.difference(neighbour)

    def sumOption(left: Int, rightOption: Option[Int]): Int = {
      rightOption match {
        case Some(right) =>
          left + right
        case None =>
          left
      }
    }

    if (average) {
      diffs.foldLeft(0)(sumOption) / diffs.size
    } else {
      diffs.min.getOrElse(0)
    }
  }

  def main (args: Array[String]): Unit = {

    val palette:ColourPalette = new ColourPalette(3)
    println(palette.colours)
    println(palette.colours.length)

    var pixels: Map[Coordinate, Option[Colour]] = (
      for { x <- List.range(0, width)
            y <- List.range(0, height) } yield new Coordinate(x, y) -> None
      ).toMap
  }
}

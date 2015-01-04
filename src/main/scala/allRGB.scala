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

  class Coordinate(val x: Int, val y: Int){
    def equals(other: Coordinate): Boolean = {
      (other.x == x) && (other.y == y)
    }
    lazy val neighbours: List[Coordinate] = (
      for {
        dy <- List.range(-1, 1)
        y1 = y + dy
        if 0 <= y1 && y1 <= height
      } yield {
        for {
          dx <- List.range(-1, 1)
          x1 = x + dx
          if 0 <= x1 && x1 <= width
        } yield {
          new Coordinate(x1, y1)
        }
    }).flatten


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

  def getAvailable(pixels: Map[Coordinate, Option[Colour]]): List[Coordinate] = {
    var ret = Set[Coordinate]()
    for {
      (coordinate, colourOption) <- pixels
      if ! isColour(colourOption)
    } for {
        neighbour <- coordinate.neighbours
        if ! ret.contains(neighbour)
        if isColour(pixels.get(neighbour) match {
          case None => None
          case Some(neighbourColourOption) =>
            neighbourColourOption match {
              case None => None
              case(neighbourColour) =>
                neighbourColourOption
            }
        })
      } ret += coordinate
    ret.toList
  }


  def main (args: Array[String]): Unit = {

    var pixels: Map[Coordinate, Option[Colour]] = (
      for { x <- List.range(0, width)
            y <- List.range(0, height) } yield new Coordinate(x, y) -> None
      ).toMap
  }
}

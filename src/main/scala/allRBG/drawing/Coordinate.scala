package allRBG.drawing

import allRBG.colour.Colour
import allRBG.constants.Constants


case class Coordinate(x: Int, y: Int) {

  assert(0 <= x && x < Constants.imageWidth)
  assert(0 <= y && y < Constants.imageHeight)

  def equals(other: Coordinate): Boolean = {
    (other.x == x) && (other.y == y)
  }

  lazy val neighbours: List[Coordinate] = {
    val rng = List(-1, 0, 1)
    (for {
      dy <- rng
      y1 = y + dy
      if 0 <= y1 && y1 < Constants.imageHeight
    } yield {
      for {
        dx <- rng
        x1 = x + dx
        if 0 <= x1 && x1 < Constants.imageWidth
      } yield {
        Coordinate(x1, y1)
      }
    }).flatten
  }

  def getNeighbourColours(pixels: Map[Coordinate, Option[Colour]]):List[Colour] = {
    for {
      neighbour <- neighbours
      if (pixels.get(neighbour) match {
        case None => false
        case Some(colourOption) => Colour.isColourOption(colourOption)
      })
    } yield pixels.get(neighbour).get.get
  }


  def calcDiff(neighbourColours: List[Colour], colour: Colour): Int = {
    val diffs: List[Int] = for {
      neighbourColour <- neighbourColours
    } yield colour.difference(neighbourColour)

    if (Constants.average) {
      diffs.sum / diffs.length
    } else {
      diffs.min
    }
  }
}
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

  def getNeighbourColours(pixels: scala.collection.mutable.Map[Coordinate, Colour]):List[Colour] = {
    for {
      neighbour <- neighbours
      colourOption = pixels.get(neighbour)
      if (colourOption match {
        case None => false
        case Some(colour) => true
      })
    } yield colourOption.get
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
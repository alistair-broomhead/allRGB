package allRBG.drawing

import allRBG.colour.Colour
import allRBG.constants.Constants


case class Coordinate(x: Int, y: Int) {
  def equals(other: Coordinate): Boolean = {
    (other.x == x) && (other.y == y)
  }

  lazy val neighbours: List[Coordinate] = {
    val rng = List(-1, 0, 1)
    (for {
      dy <- rng
      y1 = y + dy
      if 0 <= y1 && y1 <= Constants.imageHeight
    } yield {
      for {
        dx <- rng
        x1 = x + dx
        if 0 <= x1 && x1 <= Constants.imageWidth
      } yield {
        Coordinate(x1, y1)
      }
    }).flatten
  }


  def calcDiff(pixels: Map[Coordinate, Option[Colour]], colour: Colour): Int = {
    val neighbourColours: List[Option[Colour]] = for {
      neighbour <- neighbours
    } yield {
      pixels.get(neighbour) match {
        case None =>
          None
        case Some(colourOption) =>
          colourOption
      }
    }

    val diffs: List[Option[Int]] = for {
      neighbourColour <- neighbourColours
      if ! (neighbourColour equals None)
    } yield colour.difference(neighbourColour)

    def sumOption(left: Int, rightOption: Option[Int]): Int = {
      rightOption match {
        case None =>
          left// + (2 << Colour.bitDepth)
        case Some(right) =>
          left + right
      }
    }

    if (Constants.average) {
      diffs.foldLeft(0)(sumOption) / diffs.size
    } else {
      diffs.min.get
    }
  }
}
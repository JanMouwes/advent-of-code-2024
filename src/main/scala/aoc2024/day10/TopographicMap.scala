package aoc2024.day10

import aoc2024.datastructures.Coordinate
import aoc2024.datastructures.Dimensions

case class TopographicMap(private val matrix: Array[Array[Int]], dimensions: Dimensions) {
  def apply(x: Int, y: Int): Int = {
    matrix(y)(x)
  }

  def neighboursOf(coordinate: Coordinate): Set[Coordinate] = {
    val cardinalDirections = Set(
      Coordinate(-1, 0), 
      Coordinate(0, -1), 
      Coordinate(0, 1),
      Coordinate(1, 0), 
    )

    cardinalDirections.map(_ + coordinate).filter(dimensions.isInBounds)
  }
}

object TopographicMap {
  def parse(raw: String): TopographicMap = {
    val linesSeq = raw.linesIterator.toSeq
    val matrix = linesSeq.map(
      line => line.split("").map(_.toInt)
    ).toArray
    TopographicMap(
      matrix = matrix,
      dimensions = Dimensions(linesSeq.head.length, linesSeq.length)
    )
  }
}

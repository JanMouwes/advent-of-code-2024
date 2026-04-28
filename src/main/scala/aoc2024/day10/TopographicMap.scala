package aoc2024.day10

import aoc2024.day06.{Coordinate, Dimensions}

case class TopographicMap(private val matrix: Array[Array[Int]], dimensions: Dimensions) {
  def apply(x: Int, y: Int): Int = {
    matrix(y)(x)
  }

  val trailheads: Set[Coordinate] = {
    for x <- 0 until dimensions.width
        y <- 0 until dimensions.height
        if this (x, y) == 0
    yield Coordinate(x, y)
  }.toSet
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
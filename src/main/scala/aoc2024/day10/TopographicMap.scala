package aoc2024.day10

import aoc2024.day06.Dimensions

case class TopographicMap(matrix: Array[Array[Int]], dimensions: Dimensions) {
  def apply(x: Int, y: Int): Int = {
    matrix(y)(x)
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
      dimensions = Dimensions(linesSeq.length, linesSeq.head.length)
    )
  }
}
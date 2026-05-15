package aoc2024.day10

import aoc2024.datastructures.{Coordinate, Dimensions, Matrix}

case class TopographicMap(private val matrix: Matrix[Int], dimensions: Dimensions) {
  def apply(x: Int, y: Int): Int = {
    matrix(x, y)
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
    val matrix = Matrix.parse(raw, _.asDigit)
    
    TopographicMap(matrix = matrix, dimensions = matrix.dimensions)
  }
}

package aoc2024.day06

case class Coordinate(x: Int, y: Int) {
  def +(that: Coordinate): Coordinate = {
    Coordinate(this.x + that.x, this.y + that.y)
  }

  def -(that: Coordinate): Coordinate = {
    Coordinate(this.x - that.x, this.y - that.y)
  }

  def *(scalar: Int): Coordinate = {
    Coordinate(this.x * scalar, this.y * scalar)
  }
}

object Coordinate {
  def fromTuple(tuple: (Int, Int)): Coordinate = {
    Coordinate(tuple._1, tuple._2)
  }
}

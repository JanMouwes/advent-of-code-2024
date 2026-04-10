package aoc2024.day06

class Coordinate(val x: Int, val y: Int) {
  def +(that: Coordinate): Coordinate = {
    Coordinate(this.x + that.x, this.y + that.y)
  }

  def -(that: Coordinate): Coordinate = {
    Coordinate(this.x - that.x, this.y - that.y)
  }

  def *(scalar: Int): Coordinate = {
    Coordinate(this.x * scalar, this.y * scalar)
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Coordinate => this.x == that.x && this.y == that.y
    case _ => false;
  }

  override def hashCode(): Int = this.toString.hashCode

  override def toString: String = s"Coordinate(${this.x}, ${this.y})"
}

object Coordinate {
  def fromTuple(tuple: (Int, Int)): Coordinate = {
    Coordinate(tuple._1, tuple._2)
  }
}

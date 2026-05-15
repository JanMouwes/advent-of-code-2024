package aoc2024.datastructures

case class Dimensions(width: Int, height: Int) {
  def isInBounds(c: Coordinate): Boolean = {
    c.x >= 0 && c.y >= 0 && c.x < this.width && c.y < this.height
  }
}

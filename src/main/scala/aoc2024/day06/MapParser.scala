package aoc2024.day06

type Obstacles = Set[Coordinate]
type GuardPosition = Coordinate
type PatrolMap = (Dimensions, Obstacles)

class Dimensions(val width: Int, val height: Int) {
  def isInBounds(c: Coordinate): Boolean = {
    c.x >= 0 && c.y >= 0 && c.x < this.width && c.y < this.height
  }
  
  private def canEqual(other: Any): Boolean = other.isInstanceOf[Dimensions]
  
  override def equals(other: Any): Boolean = other match {
    case that: Dimensions =>
      that.canEqual(this) &&
        width == that.width &&
        height == that.height
    case _ => false
  }
  
  override def hashCode(): Int = {
    val state = Seq(width, height)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class Coordinate(val x: Int, val y: Int) {
  def +(that: Coordinate): Coordinate = {
    Coordinate(this.x + that.x, this.y + that.y)
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


def parseMap(input: String): (GuardPosition, PatrolMap) = {
  val input2d = input.linesIterator.toSeq
  val dimensions = Dimensions(input2d.head.length, input2d.size)

  val (obstacles, maybeGuardPosition) = scanMap(input2d).foldLeft[(Obstacles, Option[GuardPosition])]((Set[Coordinate](), None)) { (state, cell) =>
    val (item, currentPos) = cell
    item match {
      case '#' => (state._1.+(currentPos), state._2)
      case '^' => (state._1, Some(currentPos))
      case _ => state
    }
  }

  maybeGuardPosition match {
    case Some(guardPosition: GuardPosition) => (guardPosition, (dimensions, obstacles))
    case None => throw new IllegalArgumentException("Map did not contain a guard")
  }
}

private def scanMap(map2d: Seq[String]): IndexedSeq[(Char, Coordinate)] = {
  for y <- map2d.indices
      x <- map2d.apply(y).indices
  yield (map2d.apply(y).apply(x), Coordinate(x, y))
}
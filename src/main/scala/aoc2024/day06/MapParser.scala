package aoc2024.day06

type Dimensions = (Int, Int)
type Obstacles = Set[Coordinate]
type GuardPosition = Coordinate
type PatrolMap = (Dimensions, Obstacles, GuardPosition)

class Coordinate(val x: Int, val y: Int) {
  def :+(that: Coordinate): Coordinate = {
    Coordinate(this.x + that.x, this.y + that.y)
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Coordinate => this.x == that.x && this.y == that.y
    case _ => false;
  }

  override def hashCode(): Int = this.toString.hashCode

  override def toString: String = s"Coordinate(${this.x}, ${this.y})"
}

def parseMap(input: String): PatrolMap = {
  val input2d = input.linesIterator.toSeq
  val dimensions = (input2d.head.length, input2d.size)

  val (obstacles, maybeGuardPosition) = scanMap(input2d).foldLeft[(Obstacles, Option[GuardPosition])]((Set[Coordinate](), None)) { (state, cell) =>
    val (item, currentPos) = cell
    item match {
      case '#' => (state._1.+(currentPos), state._2)
      case '^' => (state._1, Some(currentPos))
      case _ => state
    }
  }

  maybeGuardPosition match {
    case Some(guardPosition: GuardPosition) => (dimensions, obstacles, guardPosition)
    case None => throw new IllegalArgumentException("Map did not contain a guard")
  }
}

private def scanMap(map2d: Seq[String]): IndexedSeq[(Char, Coordinate)] = {
  for y <- map2d.indices
      x <- map2d.apply(y).indices
  yield (map2d.apply(y).apply(x), Coordinate(x, y))
}
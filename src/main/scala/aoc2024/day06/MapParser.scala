package aoc2024.day06

type Obstacles = Set[Coordinate]
type GuardPosition = Coordinate
type PatrolMap = (Dimensions, Obstacles)

case class Dimensions(width: Int, height: Int) {
  def isInBounds(c: Coordinate): Boolean = {
    c.x >= 0 && c.y >= 0 && c.x < this.width && c.y < this.height
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
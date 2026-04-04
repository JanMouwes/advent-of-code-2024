package aoc2024.day06

def solvePart1(input: String): String = {
  val (start, patrolMap) = parseMap(input)
  val path = runPatrol(start, patrolMap).steps.map(_._1).toSeq.distinct

  path.iterator.size.toString
}

def solvePart2(input: String): String = {
  val (start, patrolMap) = parseMap(input)
  val obstructions = findLoopingObstructions(start, patrolMap)
  obstructions.size.toString
}

def findLoopingObstructions(start: Coordinate, patrolMap: PatrolMap): Set[Coordinate] = {
  val pathWithDirections = runPatrolWithDirections(start, patrolMap).steps.toSeq
  val obstacles = patrolMap._2

  pathWithDirections.foldLeft(SegmentedPatrolPath.empty, Set[Coordinate]()) {
    (agg, currentHeading) => {
      val (prevPath, obstacleLocations) = agg

      val (coord, dir) = currentHeading
      val potentialObstacleLocation = coord + dir.unitVector
      val nextPath = patrolFrom(
        coord,
        dir.turnRight(),
        (patrolMap._1, patrolMap._2)
      )

      val intersects = nextPath.overlaps(prevPath)

      (
        prevPath.extend(currentHeading),
        if intersects
        then obstacleLocations + potentialObstacleLocation
        else obstacleLocations
      )
    }
  }._2.removedAll(obstacles)
}
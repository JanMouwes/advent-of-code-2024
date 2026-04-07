package aoc2024.day06

def findLoopingObstructions(start: Coordinate, patrolMap: PatrolMap): Set[Coordinate] = {
  val (dimensions, obstacles) = patrolMap

  val pathWithDirections =
    runPatrolWithDirections(start, patrolMap).steps
      .sliding(2)
      .filter(window => window.head._2 == window.last._2)
      .map(_.head)
      .toSeq

  val (startCoord, startDirection) = pathWithDirections.head

  val reportProgress = false

  pathWithDirections.zipWithIndex.foldLeft(SegmentedPatrolPath.empty, Set[Coordinate]()) {
    (agg, fold) => {
      if (reportProgress) {
        val (_, i) = fold

        println(s"${i + 1} / ${pathWithDirections.size}")
      }

      val (currentHeading, _) = fold
      val (prevPath, loopingObstacles) = agg

      val (coord, dir) = currentHeading
      val potentialObstacleLocation = coord + dir.unitVector


      val nextPath = patrolFrom(
        startCoord,
        startDirection,
        (dimensions, obstacles + potentialObstacleLocation)
      )

      val intersects = nextPath.loops

      (
        prevPath.extend(currentHeading),
        if intersects
        then loopingObstacles + potentialObstacleLocation
        else loopingObstacles
      )
    }
  }._2.removedAll(obstacles + startCoord)
}
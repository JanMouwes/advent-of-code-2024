package aoc2024.day06

import scala.annotation.tailrec

enum Direction(val unitVector: Coordinate):
  case North extends Direction(Coordinate(0, -1))
  case East extends Direction(Coordinate(1, 0))
  case South extends Direction(Coordinate(0, 1))
  case West extends Direction(Coordinate(-1, 0))

  def turnRight(): Direction = this match {
    case North => East
    case East => South
    case South => West
    case West => North
  }

def runPatrol(start: Coordinate, patrolMap: PatrolMap): PatrolPath = runPatrolWithDirections(start, patrolMap)

def runPatrolWithDirections(start: Coordinate, patrolMap: PatrolMap): PatrolPath = patrolFrom(start, Direction.North, patrolMap)

def patrolFrom(from: Coordinate, direction: Direction, patrolMap: PatrolMap): PatrolPath = {
  patrolFromTailRec(from, direction, patrolMap)
}


private def patrolFromTailRec(start: Coordinate, startDirection: Direction, patrolMap: PatrolMap): PatrolPath = {
  val (dimensions, _) = patrolMap

  @tailrec
  def f(from: Coordinate, direction: Direction, agg: PatrolPath): PatrolPath = {
    val (nextCoord, nextDirection) = step(from, direction, patrolMap)

    if (!dimensions.isInBounds(nextCoord)) {
      // close off the final segment
      return agg.extend(from, direction)
    }
    if (agg.loops) {
      return agg
    }

    val newAgg =
      if nextDirection == direction
      then agg
      else agg.extend(nextCoord, direction).extend(nextCoord, nextDirection)

    f(nextCoord, nextDirection, newAgg)
  }

  f(start, startDirection, SegmentedPatrolPath.empty.extend(start, startDirection))
}

def step(coordinate: Coordinate, direction: Direction, map: PatrolMap): (Coordinate, Direction) = {
  val (dimensions, obstacles) = map
  val next = coordinate + direction.unitVector
  val isObstructed = map._2.contains(next)

  if isObstructed
  then (coordinate, direction.turnRight())
  else (next, direction)
}

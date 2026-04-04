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
    if (!dimensions.isInBounds(from)) {
      return agg
    }

    val (nextCoord, nextDirection) = step(from, direction, patrolMap)

    f(nextCoord, nextDirection, agg.extend(from, direction))
  }

  f(start, startDirection, StepPatrolPath.empty)
}

private def patrolToPath(start: Coordinate, startDirection: Direction, patrolMap: PatrolMap): PatrolPath = {
  val (dimensions, _) = patrolMap

  @tailrec
  def f(from: Coordinate, direction: Direction, agg: SegmentedPatrolPath): SegmentedPatrolPath = {
    if (!dimensions.isInBounds(from)) {
      return agg
    }

    val (nextCoord, nextDirection) = step(from, direction, patrolMap)

    f(nextCoord, nextDirection, agg.extend((from, direction)))
  }

  f(start, startDirection, SegmentedPatrolPath.empty)
}


def step(coordinate: Coordinate, direction: Direction, map: PatrolMap): (Coordinate, Direction) = {
  val (dimensions, obstacles) = map
  val next = coordinate + direction.unitVector
  val isObstructed = map._2.contains(next)

  if isObstructed
  then (coordinate, direction.turnRight())
  else (next, direction)
}

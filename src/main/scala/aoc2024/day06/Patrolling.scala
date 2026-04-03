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

def runPatrol(start: Coordinate, patrolMap: PatrolMap): Seq[Coordinate] = runPatrolWithDirections(start, patrolMap).map(_._1).distinct

def runPatrolWithDirections(start: Coordinate, patrolMap: PatrolMap): Seq[(Coordinate, Direction)] = patrolFrom(start, Direction.North, patrolMap).distinct

def patrolFrom(from: Coordinate, direction: Direction, patrolMap: PatrolMap): Seq[(Coordinate, Direction)] = {
  return patrolFromTailRec(from, direction, patrolMap)

  val (dimensions, _) = patrolMap

  if (!dimensions.isInBounds(from)) {
    return Seq()
  }

  val (nextCoord, nextDirection) = step(from, direction, patrolMap)

  (from, direction) +: patrolFrom(nextCoord, nextDirection, patrolMap)
}


def patrolFromTailRec(start: Coordinate, startDirection: Direction, patrolMap: PatrolMap): Seq[(Coordinate, Direction)] = {
  val (dimensions, _) = patrolMap

  @tailrec
  def f(from: Coordinate, direction: Direction, agg: Seq[(Coordinate, Direction)]): Seq[(Coordinate, Direction)] = {
    if (!dimensions.isInBounds(from)) {
      return agg
    }

    val (nextCoord, nextDirection) = step(from, direction, patrolMap)

    f(nextCoord, nextDirection, agg :+ (from, direction))
  }

  f(start, startDirection, Seq())
}


def step(coordinate: Coordinate, direction: Direction, map: PatrolMap): (Coordinate, Direction) = {
  val (dimensions, obstacles) = map
  val next = coordinate + direction.unitVector
  val isObstructed = map._2.contains(next)

  if isObstructed
  then (coordinate, direction.turnRight())
  else (next, direction)
}

package aoc2024.day06

import scala.annotation.tailrec

def solvePart1(input: String): String = {
  val path = runPatrol(parseMap(input))

  path.size.toString
}

def solvePart2(input: String): String = {
  ""
}

def runPatrol(patrolMap: PatrolMap): Set[Coordinate] = {
  @tailrec
  def doSteps(patrolMap: PatrolMap, direction: Direction, agg: Set[Coordinate]): Set[Coordinate] = {
    val (dimensions, _, guardPos) = patrolMap

    if (guardPos.x >= dimensions._1 || guardPos.y >= dimensions._2 || guardPos.x < 0 || guardPos.y < 0) {
      return agg
    }

    val (nextMap, nextDirection) = step(patrolMap, direction)

    doSteps(nextMap, nextDirection, agg + guardPos)
  }

  doSteps(patrolMap, Direction.North, Set())
}

enum Direction(val unitVector: Coordinate):
  case North extends Direction(Coordinate(0, -1))
  case East extends Direction(Coordinate(1, 0))
  case South extends Direction(Coordinate(0, 1))
  case West extends Direction(Coordinate(-1, 0))

  def turnRight(): Direction = {
    this match {
      case North => East
      case East => South
      case South => West
      case West => North
    }
  }

def step(map: PatrolMap, direction: Direction): (PatrolMap, Direction) = {
  val (dimensions, obstacles, guardPosition) = map
  val next = guardPosition :+ direction.unitVector
  val isObstructed = map._2.contains(next)

  if isObstructed
  then (map, direction.turnRight())
  else ((dimensions, obstacles, next), direction)
}
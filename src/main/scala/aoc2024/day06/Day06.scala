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

package aoc2024.day10

def solvePart1(input: String): String = {
  val trailMap = TrailMap(TopographicMap.parse(input))
  
  trailMap.score().toString
}

def solvePart2(input: String): String = {
  val trailMap = TrailMap(TopographicMap.parse(input))

  trailMap.rating().toString
}

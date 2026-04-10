package aoc2024.day08

def solvePart1(input: String): String = {
  parseMap(input) match {
    case Some(parsed) => findAllAntinodesOnMap(parsed.antennas, parsed.dimensions).size.toString
    case None => throw new IllegalArgumentException()
  }
}

def solvePart2(input: String): String = {
  ""
}
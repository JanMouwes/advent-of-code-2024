package aoc2024.day11

def solvePart1(input: String): String = {
  val stoneLine = StoneLine.parse(input)

  val result = stoneLine.computeNumberOfStonesAfterNChanges(25)

  result.toString
}

def solvePart2(input: String): String = {
  val stoneLine = StoneLine.parse(input)

  val result = stoneLine.computeNumberOfStonesAfterNChanges(75)

  result.toString
}

def computeStonesAfterNChanges(stoneLine: StoneLine, n: Int) = {
  val result = Seq.range(0, n).foldLeft(stoneLine) {
    (stoneLine, iter) => {
      println(s"${iter + 1} / $n")
      stoneLine.change()
    }
  }

  result.stones.size
}

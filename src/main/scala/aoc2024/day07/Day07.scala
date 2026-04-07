package aoc2024.day07

def solvePart1(input: String): String = {
  parseEquations(input) match {
    case Some(partialEquations) =>
      partialEquations
        .map(reconstructEquation(_, Seq(Operator.Plus, Operator.Mul)))
        .flatten()
        .map(_.result)
        .sum.toString
    case None => throw new IllegalArgumentException()
  }
}

def solvePart2(input: String): String = {
  parseEquations(input) match {
    case Some(partialEquations) =>
      partialEquations
        .map(reconstructEquation(_, Seq(Operator.Plus, Operator.Mul, Operator.Concat)))
        .flatten()
        .map(_.result)
        .sum.toString
    case None => throw new IllegalArgumentException()
  }
}
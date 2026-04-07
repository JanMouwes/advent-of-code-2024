package aoc2024.day07

import aoc2024.base.AocSpecBase

class CalibrationEquationParserSpec extends AocSpecBase {
  behavior of "solvePart2"

  it should "contain all equations" in {
    val input = "190: 10 19\n3267: 81 40 27"
    val expected = Seq((190, List(10, 19)), (3267, List(81, 40, 27)))

    val actual = parseEquations(input)

    actual should equal(Some(expected))
  }
}

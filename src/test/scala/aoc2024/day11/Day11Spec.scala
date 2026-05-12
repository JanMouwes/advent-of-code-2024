package aoc2024.day11

import aoc2024.base.AocSpecBase

class Day11Spec extends AocSpecBase {
  private val testInput = "125 17"
  private val realInput = "30 71441 3784 580926 2 8122942 0 291"
  
  behavior of "solvePart1"

  it should "solve the test correctly" in {
    val expected = "55312"

    val actual = solvePart1(testInput)

    actual should equal(expected)
    actual should not equal ""
  }
  it should "solve the real input correctly" in {
    val expected = "191690"

    val actual = solvePart1(realInput)

    actual should equal(expected)
    actual should not equal ""
  }
  
  behavior of "solvePart2"

  it should "solve the test correctly" in {
    val expected = "65601038650482"

    val actual = solvePart2(testInput)

    actual should equal(expected)
    actual should not equal ""
  }
  it should "solve the real input correctly" in {
    val expected = "228651922369703"

    val actual = solvePart2(realInput)

    actual should equal(expected)
    actual should not equal ""
  }
}

package aoc2024.day09

import aoc2024.base.AocSpecBase

class Day09Spec extends AocSpecBase {
  private val testInput = ""
  private val realInput = ""

  behavior of "solvePart1"

  it should "solve the test correctly" in {
    val expected = "1928"

    val actual = solvePart1(testInput)

    actual should equal(expected)
    actual should not equal ""
  }
  it should "solve the real input correctly" in {
    val expected = ""

    val actual = solvePart1(realInput)

    actual should equal(expected)
    actual should not equal ""
  }

  behavior of "solvePart2"

  it should "solve the test correctly" in {
    val expected = ""

    val actual = solvePart2(testInput)

    actual should equal(expected)
    actual should not equal ""
  }
  it should "solve the real input correctly" in {
    val expected = ""

    val actual = solvePart2(realInput)

    actual should equal(expected)
    actual should not equal ""
  }
}

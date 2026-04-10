package aoc2024.day08

import aoc2024.base.AocSpecBase

class Day08Spec extends AocSpecBase {
  private val testInput = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"
  private val realInput = ""

  behavior of "solvePart1"

  it should "solve the test correctly" in {
    val expected = ""

    val actual = solvePart1(testInput)

    actual should equal(expected)
    actual should not equal ("")
  }
  it should "solve the real input correctly" in {
    val expected = ""

    val actual = solvePart1(realInput)

    actual should equal (expected)
    actual should not equal ("")
  }

  behavior of "solvePart2"

  it should "solve the test correctly" in {
    val expected = ""

    val actual = solvePart2(testInput)

    actual should equal(expected)
  }

  it should "solve the real input correctly" in {
    val expected = ""

    val actual = solvePart2(realInput)

    actual should equal(expected)
  }
}

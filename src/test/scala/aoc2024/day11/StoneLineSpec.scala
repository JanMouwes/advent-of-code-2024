package aoc2024.day11

import aoc2024.base.TableSpecBase

class StoneLineSpec extends TableSpecBase {
  private val puzzleInput = "30 71441 3784 580926 2 8122942 0 291"

  behavior of "StoneLine.parse"

  it should "yield a non-empty StoneLine" in {
    val actual = StoneLine.parse(puzzleInput)

    actual.stones should not be empty
  }

  behavior of "StoneLine.change"

  it should "change a stone engraved with '0' to '1'" in {
    val input = StoneLine.parse("0")

    val actual = input.change()

    actual.toString shouldEqual "1"
  }

  it should "split an even-digited number into two" in {
    val input = StoneLine.parse("10")

    val actual = input.change()

    actual.toString shouldEqual "1 0"
  }

  it should "multiply a non-zero non-even-digited number by 2024" in {
    val input = StoneLine.parse("125")

    val actual = input.change()

    actual should not equal input
    actual.toString should equal(s"${125 * 2024}")
  }

  it should "correctly solve the test case" in {
    val input = StoneLine.parse("125 17")

    val actual = input.change()

    actual.toString should equal(s"${125 * 2024} 1 7")
  }

  behavior of "Stone.parse"

  it should "parse a number" in {
    val input = "1"

    val actual = Stone.parse(input)

    actual shouldBe a[Long]
  }

  it should "err on a non-numeric input" in {
    val inputs = Seq("a")

    inputs.foreach(input => {
      assertThrows[NumberFormatException] {
        val actual = Stone.parse(input)
      }
    })
  }
}

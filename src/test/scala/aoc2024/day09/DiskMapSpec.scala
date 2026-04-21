package aoc2024.day09

import aoc2024.base.AocSpecBase

class DiskMapSpec extends AocSpecBase {
  behavior of "parseDiskMap"

  it should "parse an empty input" in {
    val input = ""
    val expected = DiskMap(Seq.empty)

    val actual = parseDiskMap(input)

    actual should equal(expected)
  }

  it should "parse the expected input" in {
    val input = "123"
    val expected = DiskMap(Seq(1, 2, 3))

    val actual = parseDiskMap(input)

    actual should equal(expected)
  }
}

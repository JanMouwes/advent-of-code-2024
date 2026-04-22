package aoc2024.day09

import aoc2024.base.AocSpecBase
import aoc2024.day09.DiskBlock.{File, Gap}

class DiskMapSpec extends AocSpecBase {
  behavior of "DiskMap.parse"

  it should "parse an empty input" in {
    val input = ""
    val expected = DiskMap(Seq.empty)

    val actual = DiskMap.parse(input)

    actual should equal(expected)
  }

  it should "parse the expected input" in {
    val input = "123"
    val expected = DiskMap(Seq(File(0, 1, 0), Gap(2, 1), File(1, 3, 3)))

    val actual = DiskMap.parse(input)

    actual should equal(expected)
  }


  behavior of "checksum"
  it should "produce the expected output" in {
    val input = DiskMap(Seq(
      File(id = 0, size = 1, startIndex = 0),
      File(id = 2, size = 1, startIndex = 1),
      File(id = 1, size = 3, startIndex = 2),
      File(id = 2, size = 2, startIndex = 5)
    ))
    val expected = 0 + 2 + (2 + 3 + 4) + (10 + 12)

    val actual = input.checksum

    actual should equal(expected)
  }

  it should "produce the expected part 2 test output" in {
    val input = DiskMap(Seq(
      File(id = 0, size = 2, startIndex = 0),
      File(id = 9, size = 2, startIndex = 2),
      File(id = 2, size = 1, startIndex = 4),
      File(id = 1, size = 3, startIndex = 5),
      File(id = 7, size = 3, startIndex = 8),
      Gap(size = 1, startIndex = 11),
      File(id = 4, size = 2, startIndex = 12),
      Gap(size = 1, startIndex = 14),
      File(id = 3, size = 3, startIndex = 15),
      Gap(size = 4, startIndex = 18),
      File(id = 5, size = 4, startIndex = 22),
      Gap(size = 1, startIndex = 26),
      File(id = 6, size = 4, startIndex = 27),
      Gap(size = 5, startIndex = 31),
      File(id = 8, size = 4, startIndex = 36),
      Gap(size = 2, startIndex = 40),
    ))
    val expected = 2858

    val actual = input.checksum

    actual should equal(expected)
  }
}

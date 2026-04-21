package aoc2024.day09

import aoc2024.base.AocSpecBase
import aoc2024.day09.DiskBlock.{File, Gap}
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

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
    val expected = DiskMap(Seq(File(0, 1), Gap(2), File(1, 3)))

    val actual = DiskMap.parse(input)

    actual should equal(expected)
  }

  behavior of "compressDiskMap"

  it should "retain total file size" in {
    val input = DiskMap(Seq(File(0, 1), Gap(2), File(1, 3)))

    val actual = compressDiskMap(input)

    actual.totalFileSize should equal(input.totalFileSize)
  }

  it should "produce the expected result" in {
    val cases = Table(
      ("input", "expected"),
      (
        DiskMap(Seq(File(0, 1), Gap(2), File(1, 3))),
        DiskMap(Seq(File(0, 1), File(1, 3)))
      ),
      (
        DiskMap(Seq(File(0, 1), Gap(4), File(1, 3))),
        DiskMap(Seq(File(0, 1), File(1, 3)))
      ),
      (
        DiskMap(Seq(File(0, 1), Gap(1), File(1, 3), Gap(2), File(2, 3))),
        DiskMap(Seq(File(0, 1), File(2, 1), File(1, 3), File(2, 2)))
      )

    )

    forAll(cases) {
      (input, expected) => {

        val actual = compressDiskMap(input)

        actual should equal(expected)
      }
    }
  }

  behavior of "checksum"

  it should "produce the expected output" in {
    val input = DiskMap(Seq(File(id = 0, size = 1), File(id = 2, size = 1), File(id = 1, size = 3), File(id = 2, size = 2)))
    val expected = 0 + 2 + (2 + 3 + 4) + (10 + 12)
    
    val actual = input.checksum
    
    actual should equal(expected)
  }
}

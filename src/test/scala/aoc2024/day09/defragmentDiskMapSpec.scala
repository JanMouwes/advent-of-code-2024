package aoc2024.day09

import aoc2024.base.AocSpecBase
import aoc2024.day09.DiskBlock.{File, Gap}
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class defragmentDiskMapSpec extends AocSpecBase {
  behavior of "defragmentDiskMap"

  it should "retain total file size" in {
    val input = DiskMap(Seq(File(0, 1, 0), Gap(2, 1), File(1, 3, 3)))

    val actual = defragmentDiskMap(input)

    actual.totalFileSize should equal(input.totalFileSize)
  }

  it should "produce the expected result" in {
    val cases = Table(
      ("input", "expected"),
      (
        DiskMap(Seq(File(0, 1, 0), Gap(2, 1), File(1, 3, 3))),
        DiskMap(Seq(File(0, 1, 0), Gap(2, 1), File(1, 3, 3)))
      ),
      (
        DiskMap(Seq(File(0, 1, 0), Gap(4, 1), File(1, 3, 5))),
        DiskMap(Seq(File(0, 1, 0), File(1, 3, 1), Gap(1, 4), Gap(3, 5)))
      ),
      (
        DiskMap(Seq(File(0, 1, 0), Gap(1, 1), File(1, 3, 2), Gap(2, 5), File(2, 3, 7))),
        DiskMap(Seq(File(0, 1, 0), Gap(1, 1), File(1, 3, 2), Gap(2, 5), File(2, 3, 7)))
      )

    )

    forAll(cases) {
      (input, expected) => {
        val actual = defragmentDiskMap(input)

        actual should equal(expected)
      }
    }
  }
}

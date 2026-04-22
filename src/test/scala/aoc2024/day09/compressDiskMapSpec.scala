package aoc2024.day09

import aoc2024.base.AocSpecBase
import aoc2024.day09.DiskBlock.{File, Gap}
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class compressDiskMapSpec extends AocSpecBase {

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
}

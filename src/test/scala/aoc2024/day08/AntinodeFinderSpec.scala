package aoc2024.day08

import aoc2024.base.AocSpecBase
import aoc2024.day06.Coordinate
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class AntinodeFinderSpec extends AocSpecBase {
  behavior of "computeAntinodes"

  it should "return an empty set for inputs smaller than two elements" in {
    val inputs = Table(
      "antennas",
      Set.empty,
      Set(Coordinate(1, 1))
    )

    forAll(inputs) {
      input => {
        val actual = computeAntinodes(input)

        actual should be(empty)
      }
    }
  }

  it should "return the correct nodes for a non-empty non-singleton set" in {
    val inputs = Table(
      ("antennas", "expected"),
      (Set(Coordinate(1, 1), Coordinate(2, 2)), Set(Coordinate(0, 0), Coordinate(3, 3))),
      (Set(Coordinate(3, 3), Coordinate(2, 2)), Set(Coordinate(1, 1), Coordinate(4, 4))),
      (
        Set(Coordinate(1, 1), Coordinate(2, 2), Coordinate(1, 2)),
        Set(
          Coordinate(0, 0), Coordinate(3, 3),
          Coordinate(1, 0), Coordinate(1, 3),
          Coordinate(0, 2), Coordinate(3, 2)
        )
      ),
    )

    forAll(inputs) {
      (input, expected) => {
        val actual = computeAntinodes(input)

        actual should equal(expected)
      }
    }
  }

  it should "not return any nodes in the input set" in {
    val inputs = Table(
      ("antennas", "expected"),
      (
        Set(Coordinate(1, 1), Coordinate(2, 2), Coordinate(3, 3)),
        Set(Coordinate(0, 0), Coordinate(3, 3))
      ),
    )

    forAll(inputs) {
      (input, expected) => {
        val actual = computeAntinodes(input)

        actual.intersect(input) should be(empty)
      }
    }
  }
}

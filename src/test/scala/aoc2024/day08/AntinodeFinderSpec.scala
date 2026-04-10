package aoc2024.day08

import aoc2024.base.AocSpecBase
import aoc2024.day06.{Coordinate, Dimensions}
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class AntinodeFinderSpec extends AocSpecBase {
  behavior of "findAllAntinodesOnMap"

  it should "return the correct coordinates" in {
    val dimensions = Dimensions(5, 5)
    val inputs = Table(
      ("Antennas", "Expected antinodes"),
      (
        Set(Antenna('a', Coordinate(1, 1)), Antenna('a', Coordinate(2, 2))),
        Set(Coordinate(0, 0), Coordinate(3, 3))
      ),
      (
        Set(Antenna('a', Coordinate(1, 1)), Antenna('b', Coordinate(2, 2))),
        Set()
      ),
      (
        Set(
          Antenna('a', Coordinate(1, 1)), Antenna('a', Coordinate(1, 2)),
          Antenna('A', Coordinate(2, 1)), Antenna('A', Coordinate(2, 2)),
        ),
        Set(
          Coordinate(1, 0), Coordinate(1, 3),
          Coordinate(2, 0), Coordinate(2, 3),
        )
      ),
    )

    forAll(inputs) {
      (antennas, expected) => {
        val actual = findAllAntinodesOnMap(antennas, dimensions)

        actual should equal(expected)
      }
    }
  }


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
  
  behavior of "computeResonantAntinodes"

  it should "return an empty set for inputs smaller than two elements" in {
    val dimensions = Dimensions(5, 5)
    val inputs = Table(
      "antennas",
      Set.empty,
      Set(Coordinate(1, 1))
    )

    forAll(inputs) {
      input => {
        val actual = computeResonantAntinodes(input, dimensions)

        actual should be(empty)
      }
    }
  }

  it should "return the correct nodes for a non-empty non-singleton set" in {
    val dimensions = Dimensions(5, 5)
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
        val actual = computeResonantAntinodes(input, dimensions)

        actual should equal(expected)
      }
    }
  }

  it should "not return any nodes in the input set" in {
    val dimensions = Dimensions(5, 5)
    val inputs = Table(
      ("antennas", "expected"),
      (
        Set(Coordinate(1, 1), Coordinate(2, 2), Coordinate(3, 3)),
        Set(Coordinate(0, 0), Coordinate(3, 3))
      ),
    )

    forAll(inputs) {
      (input, expected) => {
        val actual = computeResonantAntinodes(input, dimensions)

        actual.intersect(input) should be(empty)
      }
    }
  }
}

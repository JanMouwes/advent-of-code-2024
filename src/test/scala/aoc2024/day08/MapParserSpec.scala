package aoc2024.day08

import aoc2024.base.AocSpecBase
import aoc2024.day06.{Coordinate, Dimensions}

class MapParserSpec extends AocSpecBase {

  behavior of "parseMap"

  it should "parse a small empty map" in {
    val input = "..."

    val maybeActual = parseMap(input)

    maybeActual should be(defined)
    val actual = maybeActual.get
    actual.dimensions should equal(Dimensions(3, 1))
    actual.antennas should be(empty)
  }

  it should "yield antennas" in {
    val input =
      """x.o
        |...
        |...""".stripMargin

    val maybeActual = parseMap(input)

    maybeActual should be(defined)
    val actual = maybeActual.get
    actual.dimensions should equal(Dimensions(3, 3))
    actual.antennas should equal(Set(
      ('x', Coordinate(0, 0)),
      ('o', Coordinate(2, 0))
    ))
  }

  it should "crash on an empty string" in {
    val input = ""

    val maybeActual = parseMap(input)

    maybeActual should not be(defined)
  }
}

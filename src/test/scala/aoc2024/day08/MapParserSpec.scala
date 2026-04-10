package aoc2024.day08

import aoc2024.base.AocSpecBase
import aoc2024.day06.{Coordinate, Dimensions}
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class MapParserSpec extends AocSpecBase {

  behavior of "parseMap"

  private val cases = Table(
    ("map", "dimensions", "antennas"),
    ("...", Dimensions(3, 1), Set.empty),
    (
      """x.o
        |...
        |...""".stripMargin,
      Dimensions(3, 3),
      Set(
        Antenna('x', Coordinate(0, 0)), 
        Antenna('o', Coordinate(2, 0))
      )
    ),
    (
      """............
        |........0...
        |.....0......
        |.......0....
        |....0.......
        |......A.....
        |............
        |............
        |........A...
        |.........A..
        |............
        |............""".stripMargin,
      Dimensions(12, 12),
      Set(
        Antenna('0', Coordinate(8, 1)),
        Antenna('0', Coordinate(5, 2)),
        Antenna('0', Coordinate(7, 3)),
        Antenna('0', Coordinate(4, 4)),
        Antenna('A', Coordinate(6, 5)),
        Antenna('A', Coordinate(8, 8)),
        Antenna('A', Coordinate(9, 9)),
      ),
    )
  )

  it should "parse the map correctly in described cases" in {
    forAll(cases) {
      (input, expectedDimensions, expectedAntennas) => {
        val maybeActual = parseMap(input)

        maybeActual should be (defined)
        val actual = maybeActual.get
        actual.dimensions should equal(expectedDimensions)
        actual.antennas should equal(expectedAntennas)
      }
    }
  }

  it should "crash on an empty string" in {
    val input = ""

    val maybeActual = parseMap(input)

    maybeActual should not be defined
  }
}

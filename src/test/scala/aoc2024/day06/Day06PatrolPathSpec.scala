package aoc2024.day06

import aoc2024.base.AocSpecBase
import org.scalatest.prop.TableDrivenPropertyChecks.forEvery
import org.scalatest.prop.Tables.Table

class Day06PatrolPathSpec extends AocSpecBase {
  behavior of "loops"

  it should "return true for looping maps" in {
    val inputs = Table(
      "map",
      """.#...
         |...#.
         |.^...
         |#....
         |..#..""".stripMargin,
      """.#...
         |...#.
         |.....
         |#^...
         |..#..""".stripMargin,
      """.#...
         |.^.#.
         |.....
         |#....
         |..#..""".stripMargin,
      """.#...
         |.^#..
         |.....
         |#....
         |.#...""".stripMargin
    )
    forEvery(inputs) {
      loopingMap =>
        val (guard, parsedMap) = parseMap(loopingMap)

        val path = patrolFrom(guard, Direction.North, parsedMap)

        path.loops should be(true)
    }
  }
}

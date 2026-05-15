package aoc2024.day10

import aoc2024.base.TableSpecBase
import aoc2024.datastructures.{Coordinate, Dimensions}

class TopographicMapSpec extends TableSpecBase {
  private val inputs = Table(
    ("map", "dimensions"),
    ("""7""", Dimensions(width = 1, height = 1)),
    (
      """12
        |34""".stripMargin, Dimensions(width = 2, height = 2)),
    ("""2222""", Dimensions(width = 4, height = 1))
  )

  behavior of "TopographicMap.parse"

  it should "yield a TopographicMap" in {
    forAll(inputs) {
      (map, expected) => {
        val actual = TopographicMap.parse(map)

        actual shouldBe a[TopographicMap]
      }
    }
  }

  it should "yield the correct dimensions" in {
    forAll(inputs) {
      (map, expected) => {
        val actual = TopographicMap.parse(map)

        actual.dimensions should equal(expected)
      }
    }
  }

  behavior of "TopographicMap.apply"

  it should "return a value at a coordinate" in {
    val subject = TopographicMap.parse(
      """12
        |34""".stripMargin)

    val expected = 1

    val cases = Seq(
      ((0, 0), 1),
      ((1, 0), 2),
      ((0, 1), 3),
      ((1, 1), 4),
    )
    cases.foreach((point, expected) => {
      val (x, y) = point
      val actual = subject(x, y)

      actual should be(expected)
    })
  }

  private val exampleMap = TopographicMap.parse(
    """0123
      |1234
      |8765
      |9876""".stripMargin)

  private val largerExampleMap = TopographicMap.parse(
    """89010123
      |78121874
      |87430965
      |96549874
      |45678903
      |32019012
      |01329801
      |10456732""".stripMargin)

  behavior of "TopographicMap.neighboursOf"

  it should "return a set of in-bounds coordinates" in {
    val map = exampleMap

    val actual = exampleMap.neighboursOf(Coordinate(1, 1))

    actual should have size 4
    actual.foreach(neighbour => {
      exampleMap.dimensions.isInBounds(neighbour) shouldBe true
    })
  }

  it should "return a set of mutual neighbours" in {
    val map = exampleMap

    val centre = Coordinate(1, 1)
    val actual = exampleMap.neighboursOf(centre)

    actual.foreach(neighbour => {
      exampleMap.neighboursOf(neighbour) should contain(centre)
    })
  }
}

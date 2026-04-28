package aoc2024.day10

import aoc2024.base.TableSpecBase
import aoc2024.day06.Dimensions

class TopographicMapSpec extends TableSpecBase {
  private val inputs = Table(
    ("map", "dimensions"),
    ("""7""", Dimensions(1, 1)),
    (
      """12
        |34""".stripMargin, Dimensions(2, 2)),
    ("""2222""", Dimensions(1, 4))
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
}

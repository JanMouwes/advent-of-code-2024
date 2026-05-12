package aoc2024.day10

import aoc2024.base.TableSpecBase
import aoc2024.day06.Coordinate

class TrailMapSpec extends TableSpecBase {
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

  private val examples = Table(
    "map",
    exampleMap,
    largerExampleMap
  )

  behavior of "TrailMap.trailheads"

  it should "return all zeroes on the map" in {
    val inputs = Table(
      ("map", "zeroes"),
      (
        exampleMap, Set(Coordinate(0, 0))),
      (
        largerExampleMap,
        Set(
          Coordinate(2, 0), Coordinate(4, 0),
          Coordinate(4, 2), Coordinate(6, 4),
          Coordinate(2, 5), Coordinate(5, 5),
          Coordinate(0, 6), Coordinate(6, 6),
          Coordinate(1, 7)
        )
      ),
    )

    forAll(inputs) {
      (map, expected) => {
        val actual = TrailMap(map).trailheads

        actual should have size expected.size
        actual should equal(expected)
      }
    }
  }

  behavior of "TrailMap.summitsReachableFrom"

  it should "return all summits registered in trails" in {
    val map = TrailMap(exampleMap)

    val actual = map.trailheads.flatMap(map.summitsReachableFrom)

    map.trails.foreach(trail => {
      map.summitsReachableFrom(trail.coordinates.head) should contain(trail.coordinates.last)
    })
  }

  behavior of "TrailMap.score"

  it should "yield a non-zero score if any trail exists" in {
    forAll(examples) {
      map => {
        val trailMap = TrailMap(map)
        val actual = trailMap.score()

        whenever(trailMap.trails.nonEmpty) {
          actual should not be 0
        }
      }
    }
  }

  it should "yield the expected score" in {
    val mapScores = Table(
      ("map", "expected score"),
      (exampleMap, 1),
      (largerExampleMap, 36)
    )
    
    forAll(mapScores) {
      (map, expectedScore) => {
        val trailMap = TrailMap(map)
        val actual = trailMap.score()

        actual shouldBe expectedScore 
      }
    }
  }

  behavior of "TrailMap.trails"

  private def shouldBeValidTrail(trail: Trail, map: TopographicMap): Unit = {
    trail.coordinates should have size 10

    trail.coordinates.sliding(2).foreach(pair => {
      val from = pair.head
      val to = pair.last

      map.neighboursOf(from) should contain(to)
    })

    Seq.range(0, 10).foreach(n => {
      val cell = trail.coordinates(n)
      map(cell.x, cell.y) shouldBe n
    })
  }

  it should "return expected number of valid trails" in {
    val cases = Table(
      ("TrailMap", "n of trails"),
      (exampleMap, 16),
      (largerExampleMap, 81),
    )

    forAll(cases) {
      (map, expectedTrails) => {
        val actual = TrailMap(map).trails

        actual shouldBe a[Set[Trail]]
        actual.size should be(expectedTrails)

        val actualTrail = actual.head
        actualTrail shouldBe a[Trail]

        actual.foreach(trail => {
          shouldBeValidTrail(trail, map)
        })
      }
    }
  }
}

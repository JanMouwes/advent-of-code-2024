package aoc2024.day08

import aoc2024.day06.Coordinate

def computeAntinodes(antennaSet: Set[Coordinate]): Set[Coordinate] = {
  val pairs = for {
    left <- antennaSet
    right <- antennaSet
    if left != right
  } yield (left, right)

  pairs.map(p => {
    val diff = p._1 - p._2
    p._1 + diff
  }).removedAll(antennaSet)
}
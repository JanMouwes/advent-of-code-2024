package aoc2024.day08

import aoc2024.day06.{Coordinate, Dimensions}

class Antenna(val frequency: Char, val coordinate: Coordinate) {
  private def canEqual(other: Any): Boolean = other.isInstanceOf[Antenna]

  override def equals(other: Any): Boolean = other match {
    case that: Antenna =>
      that.canEqual(this) &&
        frequency == that.frequency &&
        coordinate == that.coordinate
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(frequency, coordinate)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

def findAllAntinodesOnMap(antennas: Set[Antenna], dimensions: Dimensions): Set[Coordinate] = {
  val groupedAntennas = antennas.groupBy(_.frequency).values

  groupedAntennas.flatMap(antennaGroup => computeAntinodes(antennaGroup.map(_.coordinate)))
    .filter(dimensions.isInBounds)
    .toSet
}

def computeAntinodes(antennaLocations: Set[Coordinate]): Set[Coordinate] = {
  val pairs = for {
    left <- antennaLocations
    right <- antennaLocations
    if left != right
  } yield (left, right)

  pairs.map(p => {
    val diff = p._1 - p._2
    p._1 + diff
  }).removedAll(antennaLocations)
}

def computeResonantAntinodes(antennaLocations: Set[Coordinate], dimensions: Dimensions): Set[Coordinate] = {
  val pairs = for {
    left <- antennaLocations
    right <- antennaLocations
    if left != right
  } yield (left, right)

  pairs.map(p => {
    val diff = p._1 - p._2
    p._1 + diff
  }).removedAll(antennaLocations)
}
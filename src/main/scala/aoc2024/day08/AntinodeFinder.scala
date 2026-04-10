package aoc2024.day08

import aoc2024.day06.{Coordinate, Dimensions}

case class Antenna(frequency: Char, coordinate: Coordinate)

def findAllAntinodesOnMap(antennas: Set[Antenna], dimensions: Dimensions, resonant: Boolean = false): Set[Coordinate] = {
  val groupedAntennas = antennas.groupBy(_.frequency).values

  val computeGroupAntinodes = if resonant
  then (group: Set[Coordinate]) => computeResonantAntinodes(group, dimensions)
  else (group: Set[Coordinate]) => computeAntinodes(group)

  groupedAntennas.flatMap(antennaGroup => computeGroupAntinodes(antennaGroup.map(_.coordinate)))
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

  pairs.flatMap(p => {
    val diff = p._2 - p._1
    LazyList.from(1)
      .map(i => p._1 + (diff * i))
      .takeWhile(dimensions.isInBounds)
  })
}
package aoc2024.day08

import aoc2024.day06.{Coordinate, Dimensions}

type AntennaSet = Set[Antenna]

class AntennaMap(val antennas: AntennaSet, val dimensions: Dimensions)

def parseMap(input: String): Option[AntennaMap] = {
  if (input.isBlank) {
    return None
  }

  val lines = input.linesIterator.toSeq

  val antennas = for {
    y <- lines.indices
    x <- 0 until lines.apply(y).length
    if {
      val symbol = lines.apply(y).apply(x)
      symbol != '.'
    }
  } yield {
    val symbol = lines.apply(y).apply(x)
    Antenna(symbol, Coordinate(x, y))
  }

  Some(AntennaMap(
    antennas.toSet,
    Dimensions(lines.head.length, lines.size)
  ))
}

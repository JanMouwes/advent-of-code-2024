package aoc2024.day10

import aoc2024.day06.Coordinate

case class TrailMap(private val topographicMap: TopographicMap) {
  val trailheads: Set[Coordinate] = {
    for x <- 0 until topographicMap.dimensions.width
        y <- 0 until topographicMap.dimensions.height
        if topographicMap(x, y) == 0
    yield Coordinate(x, y)
  }.toSet

  def summitsReachableFrom(from: Coordinate): Set[Coordinate] = {
    trailsFrom(from).map(_.coordinates.last)
  }
  
  def score(): Int = {
    val summits = trailheads.toSeq.map(summitsReachableFrom)

    summits.map(_.size).sum
  }
 
  def trails: Set[Trail] = {
    trailheads.flatMap(trailsFrom)
  }

  private def trailsFrom(from: Coordinate): Set[Trail] = {
    def nextTrails(from: Coordinate, height: Int, acc: Seq[Coordinate]): Set[Trail] = {
      val max = 9

      if height == max
      then Set(Trail(acc))
      else {
        val validNeighbours =
          topographicMap
            .neighboursOf(acc.last)
            .filter(neigh => topographicMap(neigh.x, neigh.y) == height + 1)

        validNeighbours.flatMap(neighbour =>
          nextTrails(neighbour, height + 1, acc :+ neighbour)
        )
      }
    }

    nextTrails(from, 0, Seq(from))
  }

  private def nextSteps(from: Coordinate): Set[Coordinate] = {
    val plusOne = topographicMap.apply(from.x, from.y) + 1
    topographicMap.neighboursOf(from).filter(c => topographicMap(c.x, c.y) == plusOne)
  }
}

case class Trail(coordinates: Seq[Coordinate]) {
}

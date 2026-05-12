package aoc2024.day11

import scala.util.matching.Regex

private object StoneChanger {
  private val changeCache = collection.mutable.Map[Long, Vector[Long]](0L -> Vector(1L))

  def changeStone(stone: Long): Vector[Long] = {
    val engraving = stone
    changeCache.getOrElseUpdate(engraving, {
      val numberString = engraving.toString
      if isEven(numberString.length)
      then {
        val half = numberString.length >> 1
        val (left, right) = numberString.splitAt(half)
        Vector(
          Stone.parse(left),
          Stone.parse(right)
        )
      }
      else Vector(engraving * 2024)
    })
  }

  private def isEven(int: Int): Boolean = int % 2 == 0

  private val stonesCache = collection.mutable.Map.empty[(Long, Int), Long]

  def stonesAfterIterations(seed: Long, iterations: Int): Long = {
    if iterations == 0
    then 1
    else stonesCache.getOrElseUpdate((seed, iterations), {
      StoneLine(changeStone(seed)).computeNumberOfStonesAfterNChanges(iterations - 1)
    })
  }
}

case class Stone(number: Long) {
  def change(): Vector[Long] = {
    StoneChanger.changeStone(this.number)
  }

  lazy val value: Long = number

  override def toString: String = number.toString
}

object Stone {
  @inline
  def parse(raw: String): Long = {
    raw.toLong
  }

  private def dropLeadingZeroes(str: String) = {
    val dropped = str.dropWhile(_ == '0')

    if dropped.isEmpty then "0" else dropped
  }
}

case class StoneParseException(msg: String) extends Throwable {
}

case class StoneLine(stones: Vector[Long]) {
  def change(): StoneLine = {
    StoneLine(stones.flatMap(StoneChanger.changeStone))
  }

  override def toString: String = stones.map(_.toString).mkString(" ")

  def computeNumberOfStonesAfterNChanges(n: Int): Long = {
    this.stones.map(StoneChanger.stonesAfterIterations(_, n)).sum
  }
}

object StoneLine {
  def parse(raw: String): StoneLine = {
    def parseStone(str: String): Long = {
      val onlyNumbers = Regex("^[0-9]+$")
      if (!str.matches(onlyNumbers.toString()))
        throw StoneParseException(s"invalid input: $str")

      Stone.parse(str)
    }

    StoneLine(raw.split(' ').map(parseStone).toVector)
  }
}
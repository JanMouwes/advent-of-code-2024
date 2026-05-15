package aoc2024.datastructures

import scala.reflect.ClassTag

case class Matrix[T](private val innerMatrix: Array[Array[T]], dimensions: Dimensions) {
  def apply(x: Int, y: Int): T = {
    innerMatrix(y)(x)
  }
}


object Matrix {
  def parse[T: ClassTag](raw: String, parseChar: Char => T = identity): Matrix[T] = {
    val linesSeq = raw.linesIterator.toSeq
    val matrix = linesSeq.map(
      line => line.split("").map(s => parseChar(s.head))
    ).toArray
    val first = if linesSeq.nonEmpty then linesSeq.head else ""

    Matrix[T](
      innerMatrix = matrix,
      dimensions = Dimensions(first.length, linesSeq.length)
    )
  }
}
package aoc2024.day04

import scala.language.postfixOps

def solvePart1(input: String): String = {
  countWordOccurrences("XMAS", input).toString
}

def countWordOccurrences(needle: String, haystack: String): Int = {
  def countBackAndForth(needle: String, haystack: String) = {
    needle.r.findAllMatchIn(haystack).size + needle.reverse.r.findAllMatchIn(haystack).size
  }

  val turned = turn90Degrees(haystack.linesIterator.toSeq).mkString("\n")

  val horizontal = countBackAndForth(needle, haystack)
  val vertical = countBackAndForth(needle, turned)
  val diagonal = countBackAndForth(needle, diagonalMatrix(haystack.linesIterator.toSeq))
    + countBackAndForth(needle, diagonalMatrix(turned.linesIterator.toSeq))

  horizontal + vertical + diagonal
}

def turn90Degrees(matrix: Seq[String]): Seq[String] = {
  matrix.map(_.reverse).transpose.map(_.mkString(""))
}

def diagonalMatrix(matrix: Seq[String]): String = {
  def diagonalLine(index: Int, matrix: Seq[String]): String = {
    val bottomRow = matrix.last
    val startOnLastRow = index >= matrix.size
    val startCoordinates = if !startOnLastRow then (0, index) else (index - (matrix.size - 1), matrix.size - 1)

    val maxPossibleLineSize = Math.min(matrix.head.length, matrix.size)
    val lineSize = Math.min(
      Math.min(index + 1, maxPossibleLineSize),
      (matrix.size + matrix.last.length - 1) - index
    )

    def charAt(x: Int, y: Int, matrix: Seq[String]) = matrix.apply(x).apply(y)

    // up and to the right
    val chars = for i <- 0 until lineSize yield charAt(startCoordinates._1 + i, startCoordinates._2 - i, matrix)

    chars.mkString("")
  }

  val totalDiagonalLines = matrix.head.length + matrix.size - 1
  val lines = for i <- 0 until totalDiagonalLines yield diagonalLine(i, matrix)

  lines.mkString("\n")
}

def solvePart2(input: String): String = {
  val patternWidth = 3
  val newlineWidth = 1
  val width = input.linesIterator.toSeq.head.length + newlineWidth - patternWidth

  val xPatterns = Seq(
    raw"""(?s)(?=M.S(.){$width}.A.(.){$width}M.S)""".r,
    raw"""(?s)(?=M.M(.){$width}.A.(.){$width}S.S)""".r,
    raw"""(?s)(?=S.S(.){$width}.A.(.){$width}M.M)""".r,
    raw"""(?s)(?=S.M(.){$width}.A.(.){$width}S.M)""".r,
  )
  val plusPatterns = Seq(
    raw"""(?s)(?=.M.(.){$width}MAS(.){$width}.S.)""".r,
    raw"""(?s)(?=.M.(.){$width}SAM(.){$width}.S.)""".r,
    raw"""(?s)(?=.S.(.){$width}MAS(.){$width}.M.)""".r,
    raw"""(?s)(?=.S.(.){$width}SAM(.){$width}.M.)""".r,
  )
  xPatterns.map(p => {
    p.findAllMatchIn(input).size
  }).sum.toString
}
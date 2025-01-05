package aoc2024


def solvePart1(input: String): String = {
  val (left, right) = getLists(input)
  val zipped = left.sorted.zip(right.sorted)
  val doFold = (sum: Int, tup: (Int, Int)) => sum + (tup._1 - tup._2).abs
  val diff: Int = zipped.foldLeft(0)(doFold)

  diff.toString
}

def solvePart2(input: String): String = {
  val (left, right) = getLists(input)
  val appearances = countOccurrences(right)

  val score = left.foldLeft(0)((acc, l) => acc + l * appearances(l))

  score.toString
}

def getLists(input: String): (Array[Int], Array[Int]) = {
  val lines = input.split('\n').filter(s => s != "")
  val tuples = lines.map(l => {
    val splitLine = l.split(' ')
    val nums = splitLine.filter(s => s != "").map(s => s.toInt)

    (nums(0), nums(1))
  })

  (tuples.map(t => t._1), tuples.map(t => t._2))
}

def countOccurrences[T](list: Array[T])(implicit ordering: Ordering[T]): Map[T, Int] = {
  list.foldLeft(Map[T, Int]().withDefaultValue(0)) {
    case (acc, item) => acc + (item -> (1 + acc(item)))
  }
}
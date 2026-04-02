package aoc2024.day05

type PageRule = (Int, Int)
type Update = Seq[Int]
type PageRulesAndUpdates = (Seq[PageRule], Seq[Update])

def parsePageRules(input: String): PageRulesAndUpdates = {
  val split = input.split("\n\n")
  val left = split.head
  val right = split.last

  (parseRules(left), parseUpdates(right))
}

private def parseRules(str: String): Seq[(Int, Int)] = {
  str.linesIterator
    .map(_.split('|'))
    .map(t => (t.head.toInt, t.last.toInt))
    .toSeq
}

private def parseUpdates(str: String): Seq[Seq[Int]] = {
  str.linesIterator.map(_.split(",").map(_.toInt).toSeq).toSeq
}
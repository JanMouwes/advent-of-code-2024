package aoc2024.day07

type ParseResult = Seq[(Int, Seq[Int])]

def parseEquations(input: String): Option[ParseResult] = {
  input.linesIterator.map(parseEquation).foldLeft(Option(Seq.empty)) {
    (agg, maybeParsed) =>
      for {
        seq <- agg
        eq <- maybeParsed
      } yield seq :+ eq
  }
}

private def parseEquation(line: String): Option[(Int, Seq[Int])] = {
  line.split(':') match
    case Array(result, equationComponents) => Some(result.toInt, equationComponents.split(' ').filter(_.nonEmpty).map(_.toInt))
    case _ => None
}

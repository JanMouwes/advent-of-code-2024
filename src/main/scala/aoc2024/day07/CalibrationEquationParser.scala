package aoc2024.day07

type ParseResult = Seq[(Long, Seq[Long])]

def parseEquations(input: String): Option[ParseResult] = {
  input.linesIterator.map(parseEquation).foldLeft(Option(Seq.empty)) {
    (agg, maybeParsed) =>
      for {
        seq <- agg
        eq <- maybeParsed
      } yield seq :+ eq
  }
}

private def parseEquation(line: String): Option[(Long, Seq[Long])] = {
  line.split(':') match
    case Array(result, equationComponents) => Some((
      result.toLong,
      equationComponents.split(' ').filter(_.nonEmpty).map(_.toLong)
    ))
    case _ => None
}

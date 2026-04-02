package aoc2024.day05

import scala.collection.MapView

type PageRuleGraph = Map[Int, Set[Int]]

def constructPageRuleGraph(rules: Seq[PageRule]): PageRuleGraph = {
  rules.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
}

package aoc2024.day05

import scala.annotation.tailrec

def solvePart1(input: String): String = {
  val (pageRules, updates) = parsePageRules(input)

  val graph = constructPageRuleGraph(pageRules)
  val correctlyOrdered = updates.filter(isCorrectlyOrdered(_, graph))

  correctlyOrdered.map(u => u.apply((u.size - 1) / 2)).sum.toString
}

def solvePart2(input: String): String = {
  val (pageRules, updates) = parsePageRules(input)

  val ruleGraph = constructPageRuleGraph(pageRules)
  val incorrectlyOrdered = updates.filter(!isCorrectlyOrdered(_, ruleGraph))

  val fixed = incorrectlyOrdered.map(fixOrdering(_, ruleGraph))

  fixed.map(u => u.apply((u.size - 1) / 2)).sum.toString
}


def isCorrectlyOrdered(update: Update, rules: PageRuleGraph): Boolean = {
  val seen = collection.mutable.Set[Int]()
  update.forall(n => {
    val mustFollow = rules.getOrElse(n, Set())

    seen.add(n)

    !mustFollow.exists(seen.contains)
  })
}

@tailrec
def fixOrdering(update: Update, rules: PageRuleGraph): Update = {
  if (isCorrectlyOrdered(update, rules)) {
    return update
  }

  val seen = collection.mutable.Set[Int]()

  val violations = findViolations(update, rules)
  val fixed = violations.foldLeft(update) {
    (agg, v) => {
      val (other, wrong) = v
      // wrong must be moved behind right

      val target = agg.indexOf(other) + 1
      val (left, restInclWrong) = agg.splitAt(agg.indexOf(wrong))
      val (middle, right) = restInclWrong.splitAt(restInclWrong.indexOf(other))
      left ++ (middle.tail :+ other :+ wrong) ++ right.tail
    }
  }

  fixOrdering(fixed, rules)
}

def findViolations(update: Update, rules: PageRuleGraph): Seq[PageRule] = {
  val seen = collection.mutable.Set[Int]()
  update.flatMap(n => {
    val mustFollow = rules.getOrElse(n, Set())

    seen.add(n)

    val violations = mustFollow.intersect(seen)

    violations.map((n, _))
  })
}
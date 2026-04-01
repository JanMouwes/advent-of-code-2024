package aoc2024.day02

def solvePart1(input: String): String = {
  val reports = parseReports(input)
  val diff: Int = reports.count(isSafe)

  diff.toString
}

def solvePart2(input: String): String = {
  val reports = parseReports(input)
  val diff: Int = reports.count(isSafeWithDampener)

  diff.toString
}


type Report = IndexedSeq[Int]

def parseReports(input: String): Seq[Report] = {
  val lines = input.split('\n').filter(s => s != "")
  val reports = lines.map(l => {
    val splitLine = l.split(' ')
    val nums = splitLine.filter(s => s != "").map(s => s.toInt)

    nums.toIndexedSeq
  })
  reports.toSeq
}


private def isSafe(report: Report): Boolean = {
  val diffWindows = report.sliding(2).map { case Seq(n1: Int, n2: Int) => n2 - n1 }.toSeq
  val isAscending = diffWindows.forall(n => n > 0)
  val isDescending = diffWindows.forall(n => n < 0)
  val safeStepThreshold = 3
  val isSmallStep = diffWindows.forall(diff => diff.abs <= safeStepThreshold)

  (isAscending || isDescending) && isSmallStep
}

def bruteForce(reports: Seq[Report]): Seq[Report] = {
  reports.filter(computeReportVariants(_).exists(isSafe))
}

private def isSafeWithDampener(report: Report): Boolean = {
  isSafe(report) || isFixable(report)
}

def isFixable(report: Report): Boolean = {
  isSafe(report.drop(1)) ||
    isSafe(report.dropRight(1)) ||
    findViolations(report).exists(i => {
      isSafe(report.patch(i, Nil, 1))
    })
}

/**
 * @return indexes of safety violations
 */
def findViolations(report: Report): Seq[Int] = {
  val safeStepThreshold = 3

  def isStepTooBig(n1: Int, n2: Int) = (n2 - n1).abs > safeStepThreshold

  val maybeHeadIndex: Option[Int] = {
    report.take(2) match {
      case IndexedSeq(n1: Int, n2: Int) if isStepTooBig(n1, n2) => Some(0)
      case _ => None
    }
  }
  val maybeTailIndex: Option[Int] = {
    report.takeRight(2) match {
      case IndexedSeq(n1: Int, n2: Int) if isStepTooBig(n1, n2) => Some(0)
      case _ => None
    }
  }

  maybeHeadIndex.concat(
    report.sliding(3).zipWithIndex.filter { case (Seq(n1: Int, n2: Int, n3: Int), i: Int) =>
      val shouldAscend = n3 > n1
      val isViolation = shouldAscend == n1 > n2 || shouldAscend == n2 > n3 || n2 == n1 || isStepTooBig(n1, n2)

      isViolation
    }.map(_._2 + 1).concat(maybeTailIndex)
  ).toSeq
}

def computeReportVariants(report: Report): Seq[Report] = {
  report.zipWithIndex.map((n, i) => {
    report.patch(i, Nil, 1)
  })
}

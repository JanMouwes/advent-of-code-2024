package aoc2024.day07

type PartialEquation = (Long, Seq[Long])

enum Operator(val apply: (l: Long, r: Long) => Long) {
  case Plus extends Operator(_ + _)
  case Mul extends Operator(_ * _)
  case Concat extends Operator((l, r) => (l.toString + r.toString).toLong)
}

class EquationReconstruction(val result: Long, val numbers: Seq[Long], val operators: Seq[Operator]) {
  assert(numbers.size == operators.size + 1, s"incorrect number of operators: should be ${numbers.size - 1}, but was ${operators.size}")

  def isValid: Boolean = this.result == evaluateEquation(numbers, operators)
}

def evaluateEquation(numbers: Seq[Long], operators: Seq[Operator]): Long = {
  val pairs = operators.zip(numbers.tail)

  pairs.foldLeft(numbers.head) {
    (agg, pair) => {
      val (op, num) = pair
      evaluateOp(agg, op, num)
    }
  }
}

private def evaluateOp(left: Long, op: Operator, right: Long): Long = {
  op.apply(left, right)
}

def reconstructEquation(eq: PartialEquation, ops: Seq[Operator]): Option[EquationReconstruction] = {
  val (expectedResult, numbers) = eq

  val choices = computeChoices(eq, Seq.empty, ops).map(EquationReconstruction(expectedResult, numbers, _))
  choices.find(eq => eq.isValid)
}

private def computeChoices(eq: PartialEquation, currentOperators: Seq[Operator], ops: Seq[Operator]): Seq[Seq[Operator]] = {
  val (expectedResult, numbers) = eq

  if (numbers.size == 1) {
    return Seq(Seq.empty)
  }

  val left = numbers.head
  val right = numbers.tail.head

  ops.flatMap(op => {
    val res = op.apply(left, right)

    val choices = computeChoices((expectedResult - res, res +: numbers.drop(2)), currentOperators :+ op, ops)

    choices.map(choice => op +: choice)
  })
}
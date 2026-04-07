package aoc2024.day07

import aoc2024.base.AocSpecBase
import aoc2024.day07.Operator.Plus
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{forAll, whenever}

class EquationReconstructorSpec extends AocSpecBase {
  behavior of "reconstructEquation"
  
  val ops = Seq(Operator.Plus, Operator.Mul)

  it should "handle a one-number case" in {
    val input: PartialEquation = (10, Seq(10))

    val actual = reconstructEquation(input, ops)

    actual should be(defined)
    actual.get.operators should be(empty)
    actual.get.numbers should be(List(10))
    actual.get.result should be(10)
    actual.get.isValid should be(true)
  }

  it should "handle a two-number case" in {
    val input: PartialEquation = (190, Seq(10, 19))

    val actual = reconstructEquation(input, ops)

    actual should be(defined)
    actual.get.operators should be(Seq(Operator.Mul))
    actual.get.numbers should be(List(10, 19))
    actual.get.result should be(190)
    actual.get.isValid should be(true)
  }

  it should "handle a three-number case" in {
    val input: PartialEquation = (3267, Seq(81, 40, 27))

    val actual = reconstructEquation(input, ops)

    actual should be(defined)
    actual.get.operators should be(Seq(Operator.Plus, Operator.Mul))
    actual.get.numbers should be(List(81, 40, 27))
    actual.get.result should be(3267)
    actual.get.isValid should be(true)
  }

  it should "handle a 1, 1, 0 case" in {
    val input: PartialEquation = (0, Seq(1, 1, 0))

    val actual = reconstructEquation(input, ops)

    actual should be(defined)
    actual.get.operators should be(Seq(Operator.Plus, Operator.Mul))
    actual.get.numbers should be(List(1, 1, 0))
    actual.get.result should be(0)
    actual.get.isValid should be(true)
  }

  it should "get a result for all valid equations" in {
    val genNumbers = Gen.listOfN(5, Gen.posNum[Long])
    val genOperators = Gen.listOfN(4, Gen.oneOf(Seq(Operator.Plus, Operator.Mul)))
    forAll(genNumbers, genOperators) { (numbers: Seq[Long], operators: Seq[Operator]) =>
      whenever(numbers.size > 1) {
        val result = evaluateEquation(numbers, operators)

        val actual = reconstructEquation((result, numbers), ops)

        actual should be(defined)
      }
    }
  }
}

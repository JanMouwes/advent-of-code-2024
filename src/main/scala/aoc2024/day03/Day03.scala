package aoc2024.day03

import scala.util.matching.Regex

type MulInstruction = (Int, Int)

def parseMuls(input: String): Seq[MulInstruction] = {
  val mulPattern: Regex = """mul\((\d+),(\d+)\)""".r

  mulPattern.findAllMatchIn(input).map(m => {
    (m.group(1).toInt, m.group(2).toInt)
  }).toSeq
}

def solvePart1(input: String): String = {
  val muls = parseMuls(input)

  muls.map(tup => tup._1 * tup._2).sum.toString
}

enum Instruction:
  case Mul(x: Int, y: Int)
  case Do, Dont

def parseInstructions(input: String): Seq[Instruction] = {
  val instrPattern: Regex = """mul\((\d+),(\d+)\)|do\(\)|don't\(\)""".r

  instrPattern
    .findAllMatchIn(input)
    .map(m => m.group(0) match
      case "do()" => Instruction.Do
      case "don't()" => Instruction.Dont
      case _ => Instruction.Mul(m.group(1).toInt, m.group(2).toInt)
    ).toSeq
}

def filterEnabled(instructions: Seq[Instruction]): Seq[Instruction] = {
  if (instructions.isEmpty) return instructions

  val enabledInstructions = instructions.takeWhile(_ != Instruction.Dont)
  val rest = instructions.dropWhile(_ != Instruction.Dont).dropWhile(_ != Instruction.Do)

  (enabledInstructions ++ filterEnabled(rest)).filter(_ != Instruction.Do)
}

def solvePart2(input: String): String = {
  val instructions = parseInstructions(input)
  val enabled = filterEnabled(instructions)

  enabled.map({ case Instruction.Mul(x, y) => x * y
  case Instruction.Do => 0
  case Instruction.Dont => 0
  }).sum.toString
}
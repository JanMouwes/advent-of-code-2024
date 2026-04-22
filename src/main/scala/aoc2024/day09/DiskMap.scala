package aoc2024.day09

import aoc2024.day09.DiskBlock.Gap

case class DiskMap(blocks: Seq[DiskBlock]) {
  def files: Seq[DiskBlock.File] = {
    blocks.collect { case f: DiskBlock.File => f }
  }

  def gaps: Seq[DiskBlock.Gap] = {
    blocks.collect { case f: DiskBlock.Gap => f }
  }

  def totalFileSize: Int = {
    this.files.map(_.size).sum
  }

  def checksum: Long = {
    val ids = words.collect { case file: DiskBlock.File => file.id }
    ids.zipWithIndex.map { case (id: Int, index: Int) => id.toLong * index }.sum
  }

  def render: String = {
    val chars = words.map {
      case file: DiskBlock.File => file.id.toString
      case gap: DiskBlock.Gap => '.'
    }
    chars.mkString
  }

  private def words: Seq[DiskBlock] = {
    blocks.flatMap {
      case file: DiskBlock.File => Seq.range(0, file.size).map(n => file.copy(size = 1, startIndex = file.startIndex + n))
      case gap: DiskBlock.Gap => Seq.range(0, gap.size).map(n => gap.copy(size = 1, startIndex = gap.startIndex + n))
    }
  }
}

object DiskMap {
  def parse(diskMap: String): DiskMap = {
    val ints = diskMap.toSeq.map(_.asDigit)

    val blocks = ints.zipWithIndex.scanLeft(Gap(0, 0)) {
      (prevBlock, sizeAndCharIndex) => {
        val (size, charIndex) = sizeAndCharIndex
        val caret = prevBlock.startIndex + prevBlock.size
        val isEven = charIndex % 2 == 0
        val id = charIndex / 2
        if isEven then DiskBlock.File(id, size, caret) else DiskBlock.Gap(size, caret)
      }
    }

    DiskMap(blocks.tail)
  }
}

enum DiskBlock(val size: Int, val startIndex: Int):
  case File(id: Int, override val size: Int, override val startIndex: Int) extends DiskBlock(size, startIndex)
  case Gap(override val size: Int, override val startIndex: Int) extends DiskBlock(size, startIndex)


def moveFileToGap(file: DiskBlock.File, gap: DiskBlock.Gap) = {
  val remaining = gap.size - file.size
  if remaining < 0 then throw new IllegalArgumentException()

  val movedFile = file.copy(startIndex = gap.startIndex)

  if remaining == 0
  then Seq(movedFile)
  else {
    val shrunkGap = DiskBlock.Gap(size = remaining, startIndex = gap.startIndex + file.size)

    Seq(
      movedFile,
      shrunkGap
    )
  }
}
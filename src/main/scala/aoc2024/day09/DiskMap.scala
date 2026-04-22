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
    val ids = words.collect { case DiskBlock.File(id, size) => id }
    ids.zipWithIndex.map { case (id: Int, index: Int) => id.toLong * index }.sum
  }

  def render: String = {
    val chars = words.map {
      case DiskBlock.File(id, size) => id.toString
      case DiskBlock.Gap(size) => '.'
    }
    chars.mkString
  }

  private def words: Seq[DiskBlock] = {
    blocks.flatMap {
      case DiskBlock.File(id, size) => Seq.fill(size)(DiskBlock.File(id, 1))
      case DiskBlock.Gap(size) => Seq.fill(size)(DiskBlock.Gap(1))
    }
  }
}

object DiskMap {
  def parse(diskMap: String): DiskMap = {
    val ints = diskMap.toSeq.map(_.asDigit)

    val blocks = ints.zipWithIndex.map((int, index) => {
      val isEven = index % 2 == 0
      val id = index / 2
      if isEven then DiskBlock.File(id, int) else DiskBlock.Gap(int)
    })

    DiskMap(blocks)
  }
}

enum DiskBlock(val size: Int):
  case File(id: Int, override val size: Int) extends DiskBlock(size)
  case Gap(override val size: Int) extends DiskBlock(size)


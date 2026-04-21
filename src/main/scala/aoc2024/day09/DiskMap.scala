package aoc2024.day09

import scala.annotation.tailrec

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
}

enum DiskBlock(val size: Int):
  case File(id: Int, override val size: Int) extends DiskBlock(size)
  case Gap(override val size: Int) extends DiskBlock(size)

def parseDiskMap(diskMap: String): DiskMap = {
  val ints = diskMap.toSeq.map(_.asDigit)

  val blocks = ints.zipWithIndex.map((int, index) => {
    val isEven = index % 2 == 0
    val id = index / 2
    if isEven then DiskBlock.File(id, int) else DiskBlock.Gap(int)
  })

  DiskMap(blocks)
}

@tailrec
def compressDiskMap(diskMap: DiskMap): DiskMap = {
  if (diskMap.gaps.isEmpty) return diskMap

  val file = diskMap.files.last
  val gap = diskMap.gaps.head

  def doCompressionStep(diskMap: DiskMap): DiskMap = {
    val remaining = gap.size - file.size
    val (leftOfFirstGap, firstGapAndRest) = diskMap.blocks.span(_ != gap)

    val constructedTail = if remaining < 0
    then {
      val fileLeftPart = DiskBlock.File(file.id, gap.size)
      val fileRightPart = DiskBlock.File(file.id, -remaining)

      val withoutGapAndFile = firstGapAndRest.tail.init

      val constructedTail =
        if withoutGapAndFile.nonEmpty
        then fileLeftPart +: withoutGapAndFile :+ fileRightPart
        else Seq(file)

      constructedTail
    }
    else {
      val newGap = DiskBlock.Gap(remaining)

      val constructedTail = file +: newGap +: firstGapAndRest.tail.init

      constructedTail
    }

    val tripEndGaps = (leftOfFirstGap ++ constructedTail).reverse.dropWhile(_.isInstanceOf[DiskBlock.Gap]).reverse
    
    DiskMap(tripEndGaps)
  }

  val result = doCompressionStep(diskMap)

  if result == diskMap
  then diskMap
  else compressDiskMap(result)
}

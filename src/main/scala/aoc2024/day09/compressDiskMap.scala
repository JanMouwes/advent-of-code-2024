package aoc2024.day09

import scala.annotation.tailrec

@tailrec
def compressDiskMap(diskMap: DiskMap): DiskMap = {
  if (diskMap.gaps.isEmpty) return diskMap

  val file = diskMap.files.last
  val gap = diskMap.gaps.head

  def doCompressionStep(diskMap: DiskMap): DiskMap = {
    val remaining = gap.size - file.size
    val (leftOfGap, gapAndRest) = diskMap.blocks.span(_ != gap)

    val constructedTail = if remaining < 0
    then {
      val fileLeftPart = DiskBlock.File(file.id, gap.size)
      val fileRightPart = DiskBlock.File(file.id, -remaining)

      val withoutGapAndFile = gapAndRest.tail.init

      val constructedTail =
        if withoutGapAndFile.nonEmpty
        then fileLeftPart +: withoutGapAndFile :+ fileRightPart
        else Seq(file)

      constructedTail
    }
    else {
      val newGap = DiskBlock.Gap(remaining)

      val constructedTail = file +: newGap +: gapAndRest.tail.init

      constructedTail
    }

    val tripEndGaps = (leftOfGap ++ constructedTail).reverse.dropWhile(_.isInstanceOf[DiskBlock.Gap]).reverse

    DiskMap(tripEndGaps)
  }

  val result = doCompressionStep(diskMap)

  if result == diskMap
  then diskMap
  else compressDiskMap(result)
}




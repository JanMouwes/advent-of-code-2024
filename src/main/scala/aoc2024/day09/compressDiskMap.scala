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
      val fileLeftPart = file.copy(size = gap.size, startIndex = gap.startIndex)
      val fileRightPart = file.copy(size = -remaining)

      val withoutGapAndFile = gapAndRest.tail.init

      val constructedTail = {
        val areGapAndFileAdjacent = withoutGapAndFile.isEmpty
        if areGapAndFileAdjacent
        then Seq(file.copy(startIndex = gap.startIndex))
        else fileLeftPart +: withoutGapAndFile :+ fileRightPart
      }

      constructedTail
    }
    else {
      val moved = moveFileToGap(file, gap)

      val constructedTail = moved ++ gapAndRest.tail.init

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




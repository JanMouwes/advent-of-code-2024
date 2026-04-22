package aoc2024.day09

import scala.annotation.tailrec

def defragmentDiskMap(diskMap: DiskMap): DiskMap = {
  val queue = diskMap.files.reverse
  defragmentFiles(diskMap, queue)
}

@tailrec
def defragmentFiles(diskMap: DiskMap, queue: Seq[DiskBlock.File]): DiskMap = {
  if (diskMap.gaps.isEmpty || queue.isEmpty) return diskMap

  val file = queue.head

  val result = tryMoveSingleFile(diskMap, file)

  defragmentFiles(result, queue.tail)
}

def tryMoveSingleFile(diskMap: DiskMap, file: DiskBlock.File): DiskMap = {
  val maybeGap = diskMap.gaps.find(gap => gap.size >= file.size && gap.startIndex < file.startIndex)

  maybeGap match {
    case Some(gap) => {
      val remaining = gap.size - file.size
      val (leftOfGap, gapAndRest) = diskMap.blocks.span(_ != gap)

      val moved = moveFileToGap(file, gap)
      
      val constructedTail = moved ++ gapAndRest.tail.init

      val tripEndGaps = (leftOfGap ++ constructedTail).reverse.dropWhile(_.isInstanceOf[DiskBlock.Gap]).reverse

      DiskMap(tripEndGaps)
    }
    case None => diskMap
  }
}
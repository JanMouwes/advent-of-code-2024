package aoc2024.day09

import aoc2024.day09.DiskBlock.Gap

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
    case Some(gap) =>
      val remaining = gap.size - file.size
      val (leftOfGap, gapAndRest) = diskMap.blocks.span(_ != gap)

      val moved = moveFileToGap(file, gap)

      val constructedTail = {
        val (leftOfFile, fileAndRest) = gapAndRest.tail.span(_ != file)

        moved ++ leftOfFile ++ Seq(Gap(size = file.size, startIndex = file.startIndex)) ++ fileAndRest.tail
      }

      val tripEndGaps = leftOfGap ++ constructedTail

      DiskMap(tripEndGaps)
    case None => diskMap
  }
}
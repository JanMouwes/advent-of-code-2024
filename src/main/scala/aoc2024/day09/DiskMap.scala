package aoc2024.day09

case class DiskMap(blocks: Seq[Int])

def parseDiskMap(diskMap: String): DiskMap = {
  val ints = diskMap.toSeq.map(_.asDigit)
  
  DiskMap(ints)
}
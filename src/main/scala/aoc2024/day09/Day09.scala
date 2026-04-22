package aoc2024.day09

def solvePart1(input: String): String = {
  val diskMap = DiskMap.parse(input)

  val compressed = compressDiskMap(diskMap)
  
  compressed.checksum.toString
}

def solvePart2(input: String): String = {
  val diskMap = DiskMap.parse(input)

  val defragmented = defragmentDiskMap(diskMap)

  defragmented.checksum.toString
}

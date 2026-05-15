package aoc2024.datastructures

import aoc2024.base.TableSpecBase

/**
 * represents a MxN matrix of characters
 */
class MatrixSpec extends TableSpecBase {

  behavior of "CharacterMatrix.parse"
  
  it should "parse an empty map" in {
    val input = ""
    
    val actual = Matrix.parse(input)
    
    actual shouldBe a[Matrix[Char]]
    actual.dimensions shouldBe Dimensions(0, 0)
  }
  
  
  it should "parse a non-empty map" in {
    val input = "1b\nc4"
    
    val actual = Matrix.parse(input)
    
    actual shouldBe a[Matrix[Char]]
    actual.dimensions shouldBe Dimensions(2, 2)
  }
}

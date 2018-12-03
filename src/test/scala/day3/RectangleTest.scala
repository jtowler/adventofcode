package day3

import day3.NoMatterHowYouSliceIt.rectangleFromString
import org.scalatest.{FlatSpec, Matchers}

class RectangleTest extends FlatSpec with Matchers {

  behavior of "Rectangle"

  it should "initialise from string" in {
    val r1 = rectangleFromString("#1 @ 1,3: 4x4")
    r1.id shouldBe 1
    r1.right shouldBe 5
    r1.top shouldBe 7
  }

  it should "correctly calculate overlap" in {
    val r1 = Rectangle(1, 0, 0, 10, 10)
    val r2 = Rectangle(2, 5, 5, 10, 10)
    val overlap = r1.overlap(r2)
    overlap shouldBe 25
  }
  it should "correctly determine no overlap" in {
    val r1 = Rectangle(1, 0, 0, 10, 10)
    val r2 = Rectangle(2, 20, 20, 10, 10)
    val overlap = r1.overlap(r2)
    overlap shouldBe 0
  }

  it should "correctly return overlap rectangle" in {
    val r1 = Rectangle(1, 0, 0, 10, 10)
    val r2 = Rectangle(2, 5, 5, 10, 10)
    val overlap = r1.overlapRectangle(r2)
    overlap.get shouldBe Rectangle(0, 5, 5, 5, 5)
  }

  it should "correctly return overlap rectangle offset from 0" in {
    val r1 = Rectangle(1, 100, 100, 20, 20)
    val r2 = Rectangle(2, 90, 110, 20, 20)
    val overlap = r1.overlapRectangle(r2)
    overlap.get shouldBe Rectangle(0, 100, 110, 10, 10)
  }

  it should "correctly return integer pairs making up rectangle" in {
    val r1 = Rectangle(1, 0, 0, 2, 2)
    val pairs = r1.getAllInches()
    val testPairs = Set((0, 0), (0, 1), (1, 0), (1, 1))
    pairs shouldBe testPairs
  }

}

package day2

import org.scalatest.{FlatSpec, Matchers}

class InventoryManagementSystemTest extends FlatSpec with Matchers {

  behavior of "difference"
  it should "return the correct number of differing characters" in {
    val s1 = "abcdefg"
    val s2 = "axcyezg"
    val difference = InventoryManagementSystem.difference(s1, s2)
    difference shouldBe 3
  }


}

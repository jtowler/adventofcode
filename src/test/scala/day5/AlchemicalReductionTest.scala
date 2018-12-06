package day5

import org.scalatest.{FlatSpec, Matchers}

class AlchemicalReductionTest extends FlatSpec with Matchers {

  behavior of "loop"
  it should "give the correct string" in {
    val test = "dabAcCaCBAcCcaDA"
    val l = AlchemicalReduction.loop(0, test)
    l.size shouldBe 10
  }
}

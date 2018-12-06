package day4

import org.scalatest.{FlatSpec, Matchers}

class ReposeRecordTest extends FlatSpec with Matchers {

  behavior of "Line"
  it should "parse correctly" in {
    val line = Line("[1518-05-12 00:46] wakes up")
    line.year shouldBe 1518
    line.month shouldBe 5
    line.day shouldBe 12
    line.hour shouldBe 0
    line.minute shouldBe 46
    line.info shouldBe "wakes up"
  }

  behavior of "ReposeRecord"
  it should "correctly sort records" in {
    val rr = new ReposeRecord(List(
      "[1518-05-12 00:46] wakes up",
      "[1518-03-18 23:57] Guard #857 begins shift"))
    val r1 = rr.records.head
    val r2 = rr.records(1)
    r1.info shouldBe "Guard #857 begins shift"
    r2.info shouldBe "wakes up"
  }

}

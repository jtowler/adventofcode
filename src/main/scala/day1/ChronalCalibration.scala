package day1

object ChronalCalibration extends App {

  val file = "/Users/jack/IdeaProjects/adventofcode/src/main/resources/day1/ChronalCalibration.txt"
  val lines = utils.IOUtils.readFile(file)

  val freqs = lines.map {
    case s if s.head == '+' => s.tail.toInt
    case s if s.head == '-' => -s.tail.toInt
    case _ => 0
  }

  val sumFreq = freqs.sum

  def inner(n: Int, acc: Int, seenFreqs: List[Int]): Int = n match {
    case i if i == freqs.size =>
      inner(0, acc, seenFreqs)
    case i =>
      val newFreq = acc + freqs(i)
      if (seenFreqs.contains(newFreq)) newFreq
      else inner(n + 1, newFreq, newFreq :: seenFreqs)
  }

  val twiceFreq = inner(0, 0, List(0))
}

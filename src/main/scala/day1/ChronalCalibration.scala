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

  def inner(input: Stream[Int], acc: Int, seenFreqs: Set[Int]): Int = input match {
    case h #:: _ if seenFreqs contains h + acc => h + acc
    case h #:: t => inner(t, h + acc, seenFreqs + (h + acc))
  }

  val twiceFreq = inner(Stream.continually(freqs).flatten, 0, Set(0))
}

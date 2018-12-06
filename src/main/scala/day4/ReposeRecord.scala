package day4

sealed trait Info

case class Wake() extends Info

case class Sleep() extends Info

case class Start(id: String) extends Info

case class Line(s: String) {
  private val pattern = "\\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})] ([\\S\\s]+)".r
  private val groups = pattern.findAllIn(s)
  val year: Int = groups.group(1).toInt
  val month: Int = groups.group(2).toInt
  val day: Int = groups.group(3).toInt
  val hour: Int = groups.group(4).toInt
  val minute: Int = groups.group(5).toInt
  val info: Info = getInfo(groups.group(6))

  private def getInfo(s: String): Info = s match {
    case "wakes up" => Wake()
    case "falls asleep" => Sleep()
    case x =>
      val r = "Guard #([0-9]+) begins shift".r
      val g = r.findAllIn(x)
      Start(g.group(1))
  }
}

case class Guard(id: String, info: List[Line] = List()) {
  def addLine(line: Line): Guard = Guard(id, info :+ line)

  def getSumMinutes: Int = {
    val daily: Map[(Int, Int, Int), Int] = info
      .groupBy(line => (line.year, line.month, line.day))
      .mapValues(lines2Minutes(_).size)
    daily.values.sum
  }

  def minuteFrequency(m: Int): Int = {
    val daily: List[Int] = info
      .groupBy(line => (line.year, line.month, line.day))
      .mapValues(lines2Minutes).flatMap(_._2).toList
    daily.count(_ == m)
  }

  def getMaxMinute: Int = {
    val daily = info
      .groupBy(line => (line.year, line.month, line.day))
      .mapValues(lines2Minutes)
      .values
      .flatten
      .groupBy(identity)
    if (daily.isEmpty)
      -1
    else
      daily
        .mapValues(_.size)
        .maxBy(_._2)
        ._1
  }

  private def lines2Minutes(l: List[Line]): List[Int] = {
    def inner(l: List[Line], acc: List[Int], sleep: Int): List[Int] = l match {
      case Nil => acc
      case h :: t => h.info match {
        case Sleep() => inner(t, acc, h.minute)
        case Wake() if sleep >= 0 =>
          val minutes = (sleep until h.minute).toList
          inner(t, acc ++ minutes, -1)
        case _ => inner(t, acc, sleep)
      }
    }

    inner(l, List(), -1)
  }

}

class ReposeRecord(lines: List[String]) {

  val records: List[Line] = lines.map(Line).sortBy(r => (r.year, r.month, r.day, r.hour, r.minute))
  val guardList: List[Guard] = populateGuardList()

  def populateGuardList(): List[Guard] = {
    def inner(lines: List[Line], guards: List[Guard], currentGuard: Guard): List[Guard] = lines match {
      case Nil => guards
      case head :: remain =>
        head.info match {
          case Start(id) if guards exists (_.id == id) =>
            val guard = guards.filter(_.id == id).head
            val index = guards.indexWhere(_.id == id)
            val newGuard = guard.addLine(head)
            inner(remain, guards.updated(index, newGuard), newGuard)
          case Start(id) =>
            val newGuard = Guard(id, List(head))
            inner(remain, guards :+ newGuard, newGuard)
          case _ =>
            val newGuard = currentGuard.addLine(head)
            val index = guards.indexWhere(_.id == currentGuard.id)
            inner(remain, guards.updated(index, newGuard), newGuard)
        }
    }

    val firstLine: Line = records.head
    val firstGuard: Guard = firstLine.info match {
      case Start(id) => Guard(id, List(firstLine))
      case _ => throw new Exception("Expected Guard to Start")
    }

    inner(records.tail, List(firstGuard), firstGuard)
  }

}

object ReposeRecord extends App {
  val file = "/Users/jack/IdeaProjects/adventofcode/src/main/resources/day4/ReposeRecord.txt"
  val lines = utils.IOUtils.readFile(file)

  val rr = new ReposeRecord(lines)
  val maxGuard = rr.guardList.maxBy(_.getSumMinutes)
  val answer1 = maxGuard.id.toInt * maxGuard.getMaxMinute


  val maxMinute = (0 until 60) maxBy {m =>
    rr.guardList.map(_.minuteFrequency(m)).max
  }

  val g2 = rr.guardList.maxBy(_.minuteFrequency(maxMinute))

  println(g2.getMaxMinute * g2.id.toInt)

}

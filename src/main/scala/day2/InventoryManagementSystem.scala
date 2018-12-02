package day2

object InventoryManagementSystem extends App {

  val file = "/Users/jack/IdeaProjects/adventofcode/src/main/resources/day2/InventoryManagementSystem.txt"
  val codes: List[String] = utils.IOUtils.readFile(file)

  def getCounts(s: String): (Boolean, Boolean) = {
    val counts: List[Int] = s.groupBy(identity).mapValues(_.length).values.toList
    (counts.contains(2), counts.contains(3))
  }

  val doesContain: List[(Boolean, Boolean)] = codes.map(getCounts)
  val (isTwo, isThree) = doesContain.unzip

  val twos: Int = isTwo.count(_ == true)
  val threes: Int = isThree.count(_ == true)

  val checkSum = twos * threes

  def difference(s1: String, s2: String): Int = {
    s1.zip(s2).map { case (c1, c2) => c1 != c2 }.count(_ == true)
  }

  def differByOne(ss: List[String]): String = {

    def innner(current: String, remain: List[String]): Option[String] = remain match {
      case Nil => None
      case h :: _ if difference(current, h) == 1 => Some(current intersect h)
      case _ :: t => innner(current, t)
    }

    ss match {
      case Nil => throw new Exception("Could not find correct ID")
      case h :: t =>
        val result = innner(h, t)
        result match {
          case None => differByOne(t)
          case Some(s) => s
        }
    }

  }

  val id = differByOne(codes)

}

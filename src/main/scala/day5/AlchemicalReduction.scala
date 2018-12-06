package day5

object AlchemicalReduction extends App{

  val input = utils.IOUtils.readResource("AlchemicalReduction.txt", 5).head

  def loop(n: Int, s: String): String = {
    if (n >= s.length - 2) s
    else (s(n), s(n + 1)) match {
      case (a, b) if a.isUpper && b.isLower && a.toLower == b.toLower
      => loop(0, s.substring(0, n) + s.substring(n + 2))
      case (a, b) if a.isLower && b.isUpper && a.toLower == b.toLower
      => loop(0, s.substring(0, n) + s.substring(n + 2))
      case _ => loop(n + 1, s)
    }
  }

  val answer1 = loop(0, input).length

  val answer2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".map { s =>
    val newInput = input.filterNot(_ == s).filterNot(_ == s.toLower).mkString("")
    loop(0, newInput).length
  }.min

}

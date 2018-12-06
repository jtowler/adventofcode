package day3

case class Rectangle(id: Int, x: Int, y: Int, width: Int, height: Int) {
  val right: Int = x + width
  val top: Int = y + height

  def overlap(other: Rectangle): Int = {
    val dx = math.min(right, other.right) - math.max(x, other.x)
    val dy = math.min(top, other.top) - math.max(y, other.y)
    if (dx >= 0 && dy >= 0) dx * dy else 0
  }

  def area(): Int = width * height

  def getAllInches: Set[(Int, Int)] = {
    val pairs = for {
      w <- x until x + width
      h <- y until y + height
    } yield (w, h)
    pairs.toSet
  }

  def overlapRectangle(other: Rectangle): Option[Rectangle] = {
    if (overlap(other) > 1) {
      val newX = math.max(x, other.x)
      val newY = math.max(y, other.y)
      val newWidth = if (x > other.x) other.right - x else right - other.x
      val newHeight = if (y > other.y) other.top - y else top - other.y
      Some(Rectangle(0, newX, newY, newWidth, newHeight))
    }
    else None
  }
}

object NoMatterHowYouSliceIt extends App {

  def rectangleFromString(s: String): Rectangle = {
    val r = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
    val i = r.findAllIn(s)
    Rectangle(i.group(1).toInt, i.group(2).toInt, i.group(3).toInt, i.group(4).toInt, i.group(5).toInt)
  }

  val lines = utils.IOUtils.readResource("NoMatterHowYouSliceIt.txt", 3)
  val rectangles = lines.map(rectangleFromString)

  val overlaps = for {
    a <- rectangles
    if rectangles.filterNot(_ == a).map(_.getAllInches).map(a.getAllInches.intersect(_).isEmpty) forall(_ == true)
  } yield a

 val nonOverlapping = overlaps.head.id

}

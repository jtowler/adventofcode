package day6

class Grid(val coords: List[LabelCoord]) {

  private val maxX = coords.map(_.x).max
  private val maxY = coords.map(_.y).max


  private def getId(x: Int, y: Int): Int = {
    val c = new Coord(x, y)
    val distances = coords.map(_.distance(c))
    val minDistance = distances.min
    val minDistances = distances.filter(_ == minDistance)
    if (minDistances.size > 1) -1
    else {
      val index = distances.indexWhere(_ == minDistance)
      coords(index).id
    }
  }

  private val grid = List.tabulate(maxX, maxY)(getId)

  private val areaMap: Map[Int, Int] = grid.flatten.groupBy(identity).mapValues(_.size).drop(-1)

  def getMaxArea: Int = {
    val ignoreList = (0 until maxX).map(getId(_, 0)) ++
    (0 until maxY).map(getId(0, _)) ++
    (0 until maxX).map(getId(_, maxY - 1)) ++
    (0 until maxY).map(getId(maxX - 1, _))
    areaMap.filterNot(ignoreList contains _._1).values.max
  }

  def distancesToOtherCoordinates: Int = {
    val allCoords = List.tabulate(maxX, maxY)(new Coord(_, _)).flatten
    val distantCoords = allCoords.filter{ c => coords.map(_.distance(c)).sum < 10000}
    distantCoords.size
  }

}

class Coord(val x: Int, val y: Int) {
  def distance(that: Coord): Int = math.abs(x - that.x) + math.abs(y - that.y)
}

case class LabelCoord(override val x: Int, override val y: Int, id: Int) extends Coord(x, y)

object LabelCoord {
  private val pattern = "([0-9]+), ([0-9]+)".r

  def apply(s: String, i: Int): LabelCoord = {
    val g = pattern.findAllIn(s)
    LabelCoord(g.group(1).toInt, g.group(2).toInt, i)
  }
}

object ChronalCoordinates extends App {
  val file = "/Users/jack/IdeaProjects/adventofcode/src/main/resources/day6/ChronalCoordinates.txt"
  val lines = utils.IOUtils.readFile(file)
  val grid = new Grid(lines.zipWithIndex.map{ case (s, i) => LabelCoord(s, i)})
  val answer1 = grid.getMaxArea
  val answer2 = grid.distancesToOtherCoordinates
}

package utils

import scala.io.Source

object IOUtils {

  private val fileDir = "/Users/jack/IdeaProjects/adventofcode/src/main/resources/"

  def readFile(filename: String): List[String] = {
    Source.fromFile(filename)
      .getLines
      .toList
  }

  def readResource(fname: String, day: Int): List[String] = readFile(f"${fileDir}day$day/$fname")

}

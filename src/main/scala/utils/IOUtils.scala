package utils

import scala.io.Source

object IOUtils {

  def readFile(filename: String): List[String] = {
    Source.fromFile(filename)
      .getLines
      .toList
  }

}

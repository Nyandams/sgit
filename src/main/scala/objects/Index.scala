package objects
import better.files._
import annotation.tailrec

case class Index(repo: File) {

  def getIndexFile: File = repo / ".sgit" / "index"

  def indexExist: Boolean = getIndexFile.exists

  /**
    * Replace the Index with a new Index that contains the information of indexMap
    * @param indexMap Map(src -> SHA-1) of the new index
    */
  def updateIndex(indexMap: Map[String, String]): Unit = {
    val indexFile = getIndexFile
    if (indexFile.exists) {
      indexFile.delete()
      indexFile.createFile()
      indexMap foreach {
        case (key, value) => indexFile.appendText(value + " " + key + "\n")
      }
    }
  }

  /**
    * Return a map corresponding to the index
    * @return Map(src -> SHA-1)
    */
  def getMapFromIndex(): Either[String, Map[String, String]] = {
    @tailrec
    def loop(
        lines: List[String],
        mapIndex: Map[String, String]
    ): Map[String, String] = {
      if (lines.nonEmpty) {
        val line = lines.head
        val lineSplit = line.split(" ")
        val newMap = mapIndex + (lineSplit(1) -> lineSplit(0))
        loop(lines.tail, newMap)
      } else {
        mapIndex
      }
    }

    if (indexExist) {
      Right(loop(getIndexFile.lines.toList, Map()))
    } else {
      Left("file index not found")
    }
  }

}

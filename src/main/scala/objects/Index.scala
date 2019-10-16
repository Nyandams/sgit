package objects
import better.files._
import util.FileTool._
import annotation.tailrec

object Index {
  /**
   * Replace the Index with a new Index that contains the information of indexMap
   * @param repo directory of the sgit repo
   * @param indexMap Map(src -> SHA-1) of the new index
   */
  def updateIndex(repo: File, indexMap: Map[String,String]) = {
    val indexFile = repo/".sgit"/"index"
    if (indexFile.exists){
      indexFile.delete()
      indexFile.createFile()
      indexMap foreach{case(key, value) => indexFile.appendText(value + " " + key + "\n")}
    } else {
      println("index not found")
    }
  }

  /**
   * Return a map corresponding to the index
   * @param repo directory of the sgit repo
   * @return Map(src -> SHA-1)
   */
  def getMapFromIndex(repo: File): Either[String, Map[String, String]] = {
    val indexFile = repo/".sgit"/"index"

    @tailrec
    def loop(lines: List[String], mapIndex: Map[String, String]): Map[String, String] = {
      if (lines.nonEmpty){
        val line = lines.head
        val lineSplit = line.split(" ")
        val newMap = mapIndex + (lineSplit(1) -> lineSplit(0))
        loop(lines.tail, newMap)
      } else {
        mapIndex
      }
    }

    if(indexFile.exists) {
      Right(loop(indexFile.lines.toList, Map()))
    } else {
      Left("file index not found")
    }
  }

}

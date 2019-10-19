package util

import better.files.File

import scala.annotation.tailrec

case class IndexTool(repo: File) {

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

  def addFilesIndex(indexMapAdded: Map[String, String]): String = {
    getMapFromIndex() match {
      case Right(mapOldIndex) => {
        val mapDiff = (mapOldIndex.toSet diff indexMapAdded.toSet).toMap
        val indexMapFinal = mapDiff ++ indexMapAdded
        updateIndex(indexMapFinal)
        ""
      }
      case Left(error) => error
    }
  }

  def rmFilesIndex(filesToDelete: List[File]): String = {
    getMapFromIndex() match {
      case Right(mapOldIndex) => {
        val relativizedDeletedFile =
          filesToDelete.map(file => repo.relativize(file).toString)

        val mapWithoutDeleted =
          mapOldIndex.filterKeys(src => !relativizedDeletedFile.contains(src))
        updateIndex(mapWithoutDeleted)
        ""
      }
      case Left(error) => error
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

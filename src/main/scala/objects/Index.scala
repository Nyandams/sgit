package objects
import better.files._
import util.FileTool._
import annotation.tailrec

object Index {
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
   * src -> SHA-1
   */
  def getMapFromIndex(repo: File): Either[Map[String, String], String] = {
    val indexFile = repo/".sgit"/"index"

    if(indexFile.exists) {
      Left(getMapFromIndexIterator(indexFile.lineIterator))
    } else {
      Right("file index not found")
    }
  }

  def getMapFromIndexIterator(iterator: Iterator[String]): Map[String, String] ={
    if (iterator.hasNext){
      val line = iterator.next()
      val lineSplit = line.split(" ")
      Map(lineSplit(1) -> lineSplit(0)) ++ getMapFromIndexIterator(iterator)
    } else {
       Map()
    }
  }



}

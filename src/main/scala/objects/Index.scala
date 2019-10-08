package objects
import better.files._
import util.FileTool._
import annotation.tailrec

object Index {
  def updateIndex(indexMap: Map[String,String]) = {
    getSgitRec() match {
      case Left(dir) => {
        var indexFile = dir/".sgit"/"index"
        if (indexFile.exists){
          indexFile.clear()
          indexMap foreach{case(key, value) => indexFile.appendText(value + " " + key + "\n")}
        } else {
          println("index not found")
        }
      }
      case Right(error) => {
        println(error)
      }
    }
  }

  /**
   * src -> SHA-1
   */
  def getMapFromIndex(): Either[Map[String, String], String] = {

    getSgitRec() match {
      case Left(dir) => {
        var indexFile = dir/".sgit"/"index"

        if(indexFile.exists) {
          Left(getMapFromIndexIterator(indexFile.lineIterator))
        } else {
          Right("file index not found")
        }
      }
      case Right(error) => Right(error)
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

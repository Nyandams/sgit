package objects

import better.files._
import util.FileTool._
import annotation.tailrec

object Blob {

  /**
   * Create Blobs and add them to the index
   * @param files
   */
  def handleBlobsAdding(files: Array[File], index: Int = 0): Map[String, String] ={
    if (index < files.length){
      handleBlobCreation(files(index)) ++ handleBlobsAdding(files, index+1)
    } else {
      Map()
    }
  }

  /**
   * Create the blob of file and return the map corresponding to the index
   * @param file
   * @return
   */
  def handleBlobCreation(file: File): Map[String, String] ={
    val textFile = file.contentAsString
    val sha = file.sha1.toLowerCase()

    getSgitRec() match {
      case Left(sgitDir) => {
        val indexPath = file.pathAsString
        val indexPathCut = indexPath.replace(sgitDir.pathAsString, "")

        val dirBlob = sha.substring(0,2)
        val nameBlob = sha.substring(2)
        (sgitDir/".sgit"/"objects"/dirBlob/nameBlob).createFileIfNotExists(createParents = true).overwrite(textFile)
        Map(indexPathCut -> nameBlob)
      }
      case Right(error) => {
        println(error)
        Map()
      }
    }
  }
}

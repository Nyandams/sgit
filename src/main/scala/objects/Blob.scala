package objects

import better.files._
import util.FileTool._

object Blob {
  def handleBlobCreation(file: File): Unit ={
    val textFile = file.contentAsString
    val sha = file.sha1.toLowerCase()

    getSgitRec() match {
      case Left(sgitDir) => {
        val indexPath = file.pathAsString
        val indexPathCut = indexPath.replace(sgitDir.pathAsString, "")

        val dirBlob = sha.substring(0,2)
        val nameBlob = sha.substring(2)
        (sgitDir/"objects"/dirBlob/nameBlob).createFileIfNotExists(true)

      }
      case Right(error) => println(error)
    }
  }
}

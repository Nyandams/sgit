package objects

import better.files._
import util.FileTool._

object Blob {
  def handleBlobCreation(file: File): Unit ={
    val textFile = file.contentAsString
    val sha = sha1(textFile)
    println(sha)
  }
}

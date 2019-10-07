package command
import util.FileTool._
import better.files._
import objects.Blob.handleBlobCreation

object Add {
  def add(filesPath: Array[String]): Unit = {
    if (isInSgit()){
      val files = filesPath.map(fp => File(fp))

      val dirs = files.filter(f => f.isDirectory)
      val directFiles = files.filter(f => f.isRegularFile)
      val filesRec = dirs.flatMap(dir => dir.listRecursively).filter(f => f.isRegularFile)

      val filesToAdd = directFiles ++ filesRec
      filesToAdd.foreach(f => handleBlobCreation(f))
    } else {
      println("You are not in a sgit repository")
    }

  }
}

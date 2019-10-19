package command
import better.files._
import objects.Blob
import objects.Index

case class Add(repo: File) {

  def add(filesPath: Array[String]): String = {
    val validFilesToAdd = getFilesToAdd(filesPath.toList)
    val indexMapAdded = Blob(repo).handleBlobsAdding(validFilesToAdd)
    Index(repo).addFilesIndex(indexMapAdded)
  }

  private def getFilesToAdd(filesPath: List[String]): List[File] ={
    val files = filesPath.map(fp => File(fp))
    val dirs = files.filter(f => f.isDirectory)
    val directFiles = files.filter(f => f.isRegularFile)
    val filesRec =
      dirs.flatMap(dir => dir.listRecursively).filter(f => f.isRegularFile)
    val filesToAdd = directFiles ++ filesRec
    // in the repo but not sgit
    filesToAdd
      .filter(f => f.pathAsString.contains(repo.pathAsString))
      .filter(f => !f.pathAsString.contains(".sgit/"))
  }
}

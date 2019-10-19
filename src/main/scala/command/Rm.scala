package command

import better.files.File
import objects.Index

case class Rm(repo: File) {
  def rm(filesPath: Array[String]): String = {
    val filesToDelete = getFilesToRm(filesPath.toList)
    filesToDelete.filter(_.exists).map(_.delete())
    Index(repo).rmFilesIndex(filesToDelete)
  }

  private def getFilesToRm(filesPath: List[String]): List[File] = {
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

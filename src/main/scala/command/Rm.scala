package command

import better.files.File
import objects.Index.{getMapFromIndex, updateIndex}

object Rm {
  def rm(repo: File, filesPath: Array[String]): String = {
    val files = filesPath.map(fp => File(fp))
    val dirs = files.filter(f => f.isDirectory)
    val directFiles = files.filter(f => f.isRegularFile)
    val filesRec =
      dirs.flatMap(dir => dir.listRecursively).filter(f => f.isRegularFile)
    val filesAlreadyDeleted = files.filter(f => f.notExists)
    val filesToDelete = directFiles ++ filesRec ++ filesAlreadyDeleted

    filesToDelete.filter(f => f.exists).map(f => f.delete())

    getMapFromIndex(repo) match {
      case Right(mapOldIndex) => {
        val relativizedDeletedFile =
          filesToDelete.map(file => repo.relativize(file).toString)

        val mapWithoutDeleted =
          mapOldIndex.filterKeys(src => !relativizedDeletedFile.contains(src))
        updateIndex(repo, mapWithoutDeleted)
        ""
      }
      case Left(error) => error
    }
  }
}

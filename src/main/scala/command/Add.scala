package command
import better.files._
import objects.Blob.handleBlobsAdding
import objects.Index

object Add {
  def add(repo: File, filesPath: Array[String]): String = {
    val files = filesPath.map(fp => File(fp))

    val dirs = files.filter(f => f.isDirectory)
    val directFiles = files.filter(f => f.isRegularFile)
    val filesRec =
      dirs.flatMap(dir => dir.listRecursively).filter(f => f.isRegularFile)
    val filesToAdd = directFiles ++ filesRec
    // in the repo but not sgit
    val validFilesToAdd = filesToAdd
      .filter(f => f.pathAsString.contains(repo.pathAsString))
      .filter(f => !f.pathAsString.contains(".sgit/"))

    val indexMapAdded = handleBlobsAdding(repo, validFilesToAdd)
    val index = Index(repo)
    index.getMapFromIndex match {
      case Right(mapOldIndex) => {
        val mapDiff = (mapOldIndex.toSet diff indexMapAdded.toSet).toMap
        val indexMapFinal = mapDiff ++ indexMapAdded
        index.updateIndex(indexMapFinal)
        ""
      }
      case Left(error) => error
    }
  }
}

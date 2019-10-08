package command
import util.FileTool._
import better.files._
import objects.Blob.handleBlobsAdding
import objects.Index._

object Add {
  // TODO : handle sgit add .
  def add(repo: File, filesPath: Array[String]): Unit = {
    val files = filesPath.map(fp => File(fp))

    val dirs = files.filter(f => f.isDirectory)
    val directFiles = files.filter(f => f.isRegularFile)
    val filesRec = dirs.flatMap(dir => dir.listRecursively).filter(f => f.isRegularFile)
    val filesToAdd = directFiles ++ filesRec
    val indexMapAdded = handleBlobsAdding(repo, filesToAdd)

    getMapFromIndex(repo) match {
      case Left(mapOldIndex) => {
        val mapDiff = (mapOldIndex.toSet diff indexMapAdded.toSet).toMap
        val indexMapFinal = indexMapAdded ++ mapDiff
        updateIndex(repo, indexMapFinal)
      }
      case Right(error) => println(error)
    }
  }
}

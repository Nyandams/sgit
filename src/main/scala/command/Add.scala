package command
import util.FileTool._
import better.files._
import objects.Blob.handleBlobsAdding
import objects.Index._

object Add {
  def add(filesPath: Array[String]): Unit = {
    if (isInSgit()){
      val files = filesPath.map(fp => File(fp))

      val dirs = files.filter(f => f.isDirectory)
      val directFiles = files.filter(f => f.isRegularFile)
      val filesRec = dirs.flatMap(dir => dir.listRecursively).filter(f => f.isRegularFile)
      val filesToAdd = directFiles ++ filesRec
      val indexMapAdded = handleBlobsAdding(filesToAdd)
      getMapFromIndex() match {
        case Left(mapOldIndex) => {
          val mapDiff = (mapOldIndex.toSet diff indexMapAdded.toSet).toMap
          val indexMapFinal = indexMapAdded ++ mapDiff
          println(indexMapFinal)
          updateIndex(indexMapFinal)
        }
        case Right(error) => println(error)
      }
    } else {
      println("You are not in a sgit repository")
    }

  }
}

package util
import better.files._

case class ObjectTool(repo: File) {
  def getObjectsDir: File = repo / ".sgit" / "objects"

  def getObjectFromSha(sha: String): File =
    getObjectsDir / sha.substring(0, 2) / sha.substring(2)

  def objectExist(sha: String): Boolean = getObjectFromSha(sha).exists

  def getObjectsStartingWith(startSha: String): List[File] = {
    if (startSha.length > 2) {
      val dirObject = getObjectsDir / startSha.substring(0, 2)
      val startNameObject = startSha.substring(2)
      if (dirObject.exists) {
        dirObject.children
          .filter(_.isRegularFile)
          .filter(_.name.startsWith(startNameObject))
          .toList
      } else {
        List()
      }
    } else {
      List()
    }
  }

  def getFileFromShaIncomplete(
      startSha: String
  ): Either[String, File] = {
    val correspondingFiles = getObjectsStartingWith(startSha)
    if (correspondingFiles.isEmpty) {
      Left(s"pathspec '$startSha' did not match any file(s) known to git")
    } else if (correspondingFiles.length == 1) {
      Right(correspondingFiles.head)
    } else {
      Left(
        s"please specify a little more, multiple files matched to '$startSha'"
      )
    }
  }

  def getFileFromSha(sha: String): Either[String, File] = {
    getFileFromShaIncomplete(sha) match {
      case Right(file) => Right(file)
      case Left(error) => Left(error)
    }
  }
}

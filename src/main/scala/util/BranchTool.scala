package util
import better.files._

object BranchTool {

  def getHeadFilePath(repo: File): Either[String, String] = {
    val headFile = (repo / ".sgit" / "HEAD")
    if (headFile.exists) {
      val headSplited = headFile.contentAsString.split(" ")
      Right(headSplited(1))
    } else {
      Left("HEAD not found")
    }
  }

  /**
    * Return the file corresponding to the content of HEAD
    * @param repo directory of the sgit repo
    * @return Either[error, File]
    */
  def getCurrentBranch(repo: File): Either[String, File] = {
    getHeadFilePath(repo) match {
      case Right(headFile) =>
        Right((repo / ".sgit" / headFile).createFileIfNotExists())
      case Left(error) => Left(error)
    }
  }
}

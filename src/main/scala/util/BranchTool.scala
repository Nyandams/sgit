package util
import better.files._

object BranchTool {

  def getHeadFilePath(repo: File): Either[String, String] = {
    val headFile = (repo / ".sgit" / "HEAD")
    if (headFile.exists){
      Right(headFile.contentAsString.split(" ")(1))
    } else {
      Left("HEAD not found")
    }
  }

  def getCurrentBranch(repo: File): Either[String, File] = {
    getHeadFilePath(repo) match {
      case Right(headFile) => Right((repo / ".sgit" / headFile).createFileIfNotExists())
      case Left(error) => Left(error)
    }
  }
}

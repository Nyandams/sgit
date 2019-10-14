package util
import better.files._

object BranchTool {

  def getHeadFilePath(repo: File): Either[String, String] = {
    val headFile = (repo / ".sgit" / "HEAD")
    if (headFile.exists){
      Left(headFile.contentAsString.split(" ")(1))
    } else {
      Right("HEAD not found")
    }
  }

  def getCurrentBranch(repo: File): Either[File, String] = {
    getHeadFilePath(repo) match {
      case Left(headFile) => Left((repo / ".sgit" / headFile).createFileIfNotExists())
      case Right(error) => Right(error)
    }
  }
}

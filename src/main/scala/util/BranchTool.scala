package util
import better.files._
import java.io.File.separator

case class BranchTool(repo: File) {

  def getHeadFile: File = repo / ".sgit" / "HEAD"

  def getRefsFolder: File = repo / ".sgit" / "refs"

  def getHeadsFolder: File = getRefsFolder / "heads"

  def getTagsFolder: File = getRefsFolder / "tags"

  def getBranchFile(nameBranch: String): File = getHeadsFolder / nameBranch

  def getTagFile(nameTag: String): File = getTagsFolder / nameTag

  def getDetachedFile: File = getRefsFolder / "detached"

  def headExist: Boolean = getHeadFile.exists

  def getRelativeCurrentHeadFilePath: Either[String, String] = {
    if (headExist) {
      Right(getHeadFile.contentAsString.split(" ")(1))
    } else {
      Left("HEAD not found")
    }
  }

  def updateHeadRefBranch(branch: String): Unit = {
    if (headExist)
      getHeadFile.overwrite(s"ref: refs${separator}heads${separator}$branch")
  }

  def updateHeadRefDetached(sha: String): Unit = {
    if (headExist) {
      getDetachedFile.createFileIfNotExists(createParents = true).overwrite(sha)
      getHeadFile.overwrite(s"ref: refs${separator}detached")
    }
  }

  /**
    * Return the file corresponding to the content of HEAD
    * @return Either[error, File]
    */
  def getCurrentHeadFile: Either[String, File] = {
    getRelativeCurrentHeadFilePath match {
      case Left(error) => Left(error)
      case Right(relativeHeadFile) =>
        Right((repo / ".sgit" / relativeHeadFile).createFileIfNotExists())
    }
  }
}

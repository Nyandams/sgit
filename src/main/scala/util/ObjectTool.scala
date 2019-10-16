package util
import better.files._

object ObjectTool {
  def getFileFromSha(repo: File, sha: String): Either[String, File] = {
    if (sha.length >= 38) {
      val dirObject = sha.substring(0,2)
      val nameObject = sha.substring(2)
      val fileObject = (repo/".sgit"/"objects"/dirObject/nameObject)
      if (fileObject.exists){
        Right(fileObject)
      } else {
        Left(s"Object ${sha} not found")
      }
    } else {
      Left(s"sha1: ${sha} not valid")
    }
  }
}

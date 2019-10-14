package util
import better.files._

object ObjectUtil {
  def getFileFromSha(repo: File, sha: String): Either[File, String] = {
    if (sha.length >= 38) {
      val dirObject = sha.substring(0,2)
      val nameObject = sha.substring(2)
      val fileObject = (repo/".sgit"/"objects"/dirObject/nameObject)
      if (fileObject.exists){
        Left(fileObject)
      } else {
        Right(s"Object ${sha} not found")
      }
    } else {
      Right(s"sha1: ${sha} not valid")
    }

  }
}

package util
import better.files._

object ObjectTool {
  def getFileFromSha(repo: File, sha: String): Either[String, File] = {
    getFileFromShaIncomplete(repo, sha) match {
      case Right(file) => Right(file)
      case Left(error) => Left(error)
    }
  }

  def getFileFromShaIncomplete(repo: File, shaCut: String): Either[String, File] = {
    if(shaCut.length >2){
      val dirObject = (repo/".sgit"/"objects"/shaCut.substring(0,2))
      val startNameObject = shaCut.substring(2)
      if(dirObject.exists){
        val filesCorresponding = dirObject.children.filter(child => child.name.startsWith(startNameObject)).toList
        if (filesCorresponding.isEmpty){
          Left(s"pathspec '${shaCut}' did not match any file(s) known to git")
        } else if (filesCorresponding.length == 1){
          Right(filesCorresponding.head)
        }else {
          Left(s"please specify a little more, multiple files matched to '${shaCut}'")
        }
      } else {
        Left(s"pathspec '${shaCut}' did not match any file(s) known to git")
      }
    } else {
      Left(s"pathspec '${shaCut}' did not match any file(s) known to git")
    }
  }
}

package command
import better.files._
import util.BranchTool.getCurrentBranch
import util.CommitTool.isThereACommit

object Tag {
  def newTag(repo: File, nameTag: String): String = {
    var toPrint = ""
    if (isThereACommit(repo)) {
      getCurrentBranch(repo) match {
        case Left(error) =>
          toPrint += "Failed to resolve 'HEAD' as a valid ref\n"
        case Right(currentBranch) =>
          val splitTagName = nameTag.split(" ")
          if (splitTagName.length == 1) {
            val tagFolder = (repo / ".sgit" / "refs" / "tags")
            if (tagFolder.exists) {
              if (tagFolder.list.contains(
                    (repo / ".sgit" / "refs" / "tags" / nameTag)
                  )) {
                toPrint += s"tag '${nameTag}' already exists\n"
              } else {
                val tagFile = (repo / ".sgit" / "refs" / "tags" / nameTag)
                  .createFileIfNotExists(true)
                tagFile.appendText(currentBranch.contentAsString)
              }
            } else {
              toPrint += "no tags directory\n"
            }
          } else {
            toPrint += s"'${nameTag}' is not a valid tag name\n"
          }
      }
    } else {
      toPrint += s"Not a valid object name: 'master'.\n"
    }
    toPrint
  }

  def showTags(repo: File): String = {
    var toPrint = ""
    val tagFolder = (repo / ".sgit" / "refs" / "tags")
    if (tagFolder.exists) {
      toPrint += (tagFolder.children.map(f => f.name) mkString "\n") + "\n"
    }
    toPrint
  }
}

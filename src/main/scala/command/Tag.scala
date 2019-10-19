package command
import better.files._
import util.BranchTool
import util.CommitTool

case class Tag(repo: File) {
  def newTag(nameTag: String): String = {
    val commitTool = CommitTool(repo)
    if (commitTool.isThereACommit) {
      val branchTool = BranchTool(repo)
      val newTag = branchTool.getTagFile(nameTag)
      if (newTag.exists){
        s"branch '${nameTag}' already exists"
      } else {
        newTag.createFileIfNotExists(true)
        newTag.appendText(commitTool.lastCommitSha.get)
        ""
      }
    } else {
      s"Not a valid object name: 'master'."
    }
  }

  def showTags: String = {
    val tagFolder = BranchTool(repo).getTagsFolder
    if (tagFolder.exists) {
      (tagFolder.children.map(f => f.name) mkString "\n")
    } else {
      ""
    }
  }
}

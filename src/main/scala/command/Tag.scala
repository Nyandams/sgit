package command
import better.files._
import util.BranchTool.getCurrentBranch
import util.CommitTool.isThereACommit

object Tag {
  def newTag(repo: File, nameTag: String ): Unit ={
    if(isThereACommit(repo)){
      getCurrentBranch(repo) match {
        case Left(error) => println("Failed to resolve 'HEAD' as a valid ref")
        case Right(currentBranch) =>
          val splitTagName = nameTag.split(" ")
          if (splitTagName.length == 1){
            val tagFolder = (repo/".sgit"/"refs"/"tags")
            if (tagFolder.exists){
              if (tagFolder.list.contains((repo/".sgit"/"refs"/"tags"/nameTag))) {
                println(s"tag '${nameTag}' already exists")
              } else {
                val tagFile = (repo/".sgit"/"refs"/"tags"/nameTag).createFileIfNotExists(true)
                tagFile.appendText(currentBranch.contentAsString)
              }
            } else {
              println("no tags directory")
            }
          } else {
            println(s"'${nameTag}' is not a valid tag name")
          }
      }
    } else {
      println(s"Not a valid object name: 'master'.")
    }

  }

  def showTags(repo: File): Unit ={
    val tagFolder = (repo/".sgit"/"refs"/"tags")
    if(tagFolder.exists){
      println(tagFolder.list.map(f => f.name) mkString "\n")
    }

  }
}

package command

import better.files._
import util.FileTool._

object Init{
  def init(path:File = getUserDirectory): Unit ={
    if (isInSgit()){
      println("You are already in a sgit repository")
    } else {
      (path/".sgit").createDirectory()
      (path/".sgit"/"objects").createDirectory()
      (path/".sgit"/"refs"/"heads").createDirectories()
      (path/".sgit"/"refs"/"tags").createDirectories()
      (path/".sgit"/"index").createFile()
      (path/".sgit"/"HEAD").createFile().overwrite("ref: refs/heads/master")
    }
  }

}

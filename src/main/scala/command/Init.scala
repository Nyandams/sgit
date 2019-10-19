package command

import util.ObjectTool
import util.BranchTool
import objects.Index
import better.files._
import util.FileTool._

case class Init(path: File = getUserDirectory) {
  def init: String = {
    if (isInSgit()) {
      "You are already in a sgit repository"
    } else {
      val objectTool = ObjectTool(path)
      val branchTool = BranchTool(path)
      val index = Index(path)
      (path / ".sgit").createDirectory()
      objectTool.getObjectsDir.createDirectory()
      branchTool.getHeadsFolder.createDirectories()
      branchTool.getTagsFolder.createDirectories()
      index.getIndexFile.createFile()
      branchTool.getHeadFile.createFile()
      branchTool.updateHeadRefBranch("master")
      ""
    }
  }

}

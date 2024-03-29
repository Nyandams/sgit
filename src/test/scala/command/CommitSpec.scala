package command
import java.io
import java.nio.file.Files
import util.FileTool.sha1Hash
import better.files._
import util.ObjectTool
import org.scalatest.{FlatSpec, BeforeAndAfterEach}


class CommitSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init(tempDirPath).init
    val f = (tempDirPath/"1").createFileIfNotExists(true)
    Add(tempDirPath).add(Array(f.pathAsString))
    val f2 = (tempDirPath/"folder"/"2").createFileIfNotExists(true)
    f2.appendText("text file 2")
    Add(tempDirPath).add(Array(f2.pathAsString))
  }

  "The commit command" should "run" in {
    Commit(tempDirPath).commit("")
  }

  it should "create the branch in .sgit/branches if it is its first commit" in {
    Commit(tempDirPath).commit("1st commit")
    assert((tempDirPath/".sgit"/"refs"/"heads"/"master").exists)
  }

  it should "update the current branch with the commit" in {
    Commit(tempDirPath).commit("1st commit")
    val lastCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    val f = (tempDirPath/"3").createFileIfNotExists(createParents = true)
    Add(tempDirPath).add(Array(f.pathAsString))
    Commit(tempDirPath).commit("2nd commit")
    val newCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    assert(lastCommit != newCommit)
  }

  it should "not create commit object if the previous commit is the same" in {
    Commit(tempDirPath).commit("1st commit")
    val lastCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    Commit(tempDirPath).commit("2nd commit")
    val newCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    assert(lastCommit == newCommit)
  }

  it should "create a commit file referenced in heads" in {
    Commit(tempDirPath).commit("1st commit")
    val lastCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    ObjectTool(tempDirPath).getFileFromSha(lastCommit) match {
      case Right(commit) => assert(commit.exists)
      case Left(error) => assert(false)
    }
  }

  it should "create a tree for folder directory containing 2" in {
    Commit(tempDirPath).commit("1st commit")

    val shaf2 = sha1Hash("text file 2")
    val treeFolderContent = s"blob ${shaf2} 2"
    val shaTree = sha1Hash(treeFolderContent)
    ObjectTool(tempDirPath).getFileFromSha(shaTree) match {
      case Right(treeFile) => assert(treeFile.exists)
      case Left(error) => assert(false)
    }
  }



}
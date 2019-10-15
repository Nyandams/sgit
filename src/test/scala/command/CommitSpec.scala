package command
import java.io
import java.nio.file.Files
import java.io.File.separator
import util.FileTool.sha1Hash
import better.files._
import objects.Index._
import util.ObjectUtil.getFileFromSha
import org.scalatest.{FlatSpec, FunSpec, Matchers, BeforeAndAfterEach}


class CommitSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
    val f = (tempDirPath/"1").createFileIfNotExists(true)
    Add.add(tempDirPath, Array(f.pathAsString))
    val f2 = (tempDirPath/"folder"/"2").createFileIfNotExists(true)
    f2.appendText("text file 2")
    Add.add(tempDirPath, Array(f2.pathAsString))
  }

  "The commit command" should "run" in {
    Commit.commit(tempDirPath, "")
  }

  it should "create the branch in .sgit/branches if it is its first commit" in {
    Commit.commit(tempDirPath, "1st commit")
    assert((tempDirPath/".sgit"/"refs"/"heads"/"master").exists)
  }

  it should "update the current branch with the commit" in {
    Commit.commit(tempDirPath, "1st commit")
    val lastCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    val f = (tempDirPath/"3").createFileIfNotExists(createParents = true)
    Add.add(tempDirPath, Array(f.pathAsString))
    Commit.commit(tempDirPath, "2nd commit")
    val newCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    assert(lastCommit != newCommit)
  }

  it should "not create commit object if the previous commit is the same" in {
    Commit.commit(tempDirPath, "1st commit")
    val lastCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    Commit.commit(tempDirPath, "2nd commit")
    val newCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    assert(lastCommit == newCommit)
  }

  it should "create a commit file referenced in heads" in {
    Commit.commit(tempDirPath, "1st commit")
    val lastCommit = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    getFileFromSha(tempDirPath, lastCommit) match {
      case Right(commit) => assert(commit.exists)
      case Left(error) => assert(false)
    }
  }

  it should "create a tree for folder directory containing 2" in {
    Commit.commit(tempDirPath, "1st commit")

    val shaf2 = sha1Hash("text file 2")
    val treeFolderContent = s"blob ${shaf2} 2"
    val shaTree = sha1Hash(treeFolderContent)
    getFileFromSha(tempDirPath, shaTree) match {
      case Right(treeFile) => assert(treeFile.exists)
      case Left(error) => assert(false)
    }
  }



}
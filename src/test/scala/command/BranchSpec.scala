package command
import java.io
import java.nio.file.Files
import better.files._
import org.scalatest.{FlatSpec, BeforeAndAfterEach}


class BranchSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init(tempDirPath).init
  }

  "The newBranch command" should "create a branch in .sgit/refs/heads with the right content in it" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Branch(tempDirPath).newBranch("test")
    val sha = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    val branchFile = (tempDirPath/".sgit"/"refs"/"heads"/"test")
    assert(branchFile.exists && branchFile.contentAsString == sha)
  }

  it should "not create a branch if there is no commit" in {
    Branch(tempDirPath).newBranch("test")
    val file = (tempDirPath/".sgit"/"refs"/"heads"/"testTag")
    assert(!file.exists)
  }

  it should "not create a branch if it already exists" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Branch(tempDirPath).newBranch("test")
    val branchFile1Content = (tempDirPath/".sgit"/"refs"/"heads"/"test").contentAsString

    val f2 = (tempDirPath/"2").createFile()
    Add(tempDirPath).add(Array(f2.pathAsString))
    Commit(tempDirPath).commit("2nd Commit")
    Branch(tempDirPath).newBranch("test")
    val branchFile2Content = (tempDirPath/".sgit"/"refs"/"heads"/"test").contentAsString
    assert(branchFile2Content == branchFile1Content)
  }

  "the showBranch command" should "return a number of line corresponding to the number of branch" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Branch(tempDirPath).newBranch("test")
    Branch(tempDirPath).newBranch("test2")
    val printLength = Branch(tempDirPath).showBranch.split("\n").length
    assert(printLength == 3)
  }

  "the showBranch command" should "all the branch created" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Branch(tempDirPath).newBranch( "test")
    Branch(tempDirPath).newBranch( "test2")
    val printArray = Branch(tempDirPath).showBranch.split("\n")
    assert(printArray.contains("  test") && printArray.contains("  test2"))
  }

  "the showBranchVerbose command" should "return a number of line corresponding to the number of branch" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Branch(tempDirPath).newBranch("test")
    Branch(tempDirPath).newBranch("test2")
    val printLength = Branch(tempDirPath).showBranchVerbose.split("\n").length
    assert(printLength == 3)
  }
}
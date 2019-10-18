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
    Init.init(tempDirPath)
  }

  "The newBranch command" should "create a branch in .sgit/refs/heads with the right content in it" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    Branch.newBranch(tempDirPath, "test")
    val sha = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    val branchFile = (tempDirPath/".sgit"/"refs"/"heads"/"test")
    assert(branchFile.exists && branchFile.contentAsString == sha)
  }

  it should "not create a branch if there is no commit" in {
    Branch.newBranch(tempDirPath, "test")
    val file = (tempDirPath/".sgit"/"refs"/"heads"/"testTag")
    assert(!file.exists)
  }

  it should "not create a branch if it already exists" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    Branch.newBranch(tempDirPath, "test")
    val branchFile1Content = (tempDirPath/".sgit"/"refs"/"heads"/"test").contentAsString

    val f2 = (tempDirPath/"2").createFile()
    Add.add(tempDirPath, Array(f2.pathAsString))
    Commit.commit(tempDirPath, "2nd Commit")
    Branch.newBranch(tempDirPath, "test")
    val branchFile2Content = (tempDirPath/".sgit"/"refs"/"heads"/"test").contentAsString
    assert(branchFile2Content == branchFile1Content)
  }

  "the showBranch command" should "return a number of line corresponding to the number of branch" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    Branch.newBranch(tempDirPath, "test")
    Branch.newBranch(tempDirPath, "test2")
    val printLength = Branch.showBranch(tempDirPath).split("\n").length
    assert(printLength == 3)
  }

  "the showBranch command" should "all the branch created" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    Branch.newBranch(tempDirPath, "test")
    Branch.newBranch(tempDirPath, "test2")
    val printArray = Branch.showBranch(tempDirPath).split("\n")
    assert(printArray.contains("  test") && printArray.contains("  test2"))
  }

  "the showBranchVerbose command" should "return a number of line corresponding to the number of branch" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    Branch.newBranch(tempDirPath, "test")
    Branch.newBranch(tempDirPath, "test2")
    val printLength = Branch.showBranchVerbose(tempDirPath).split("\n").length
    assert(printLength == 3)
  }
}
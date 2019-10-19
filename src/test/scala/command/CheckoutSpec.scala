package command
import java.io
import java.nio.file.Files
import better.files._
import objects.Index
import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.BranchTool

class CheckoutSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init(tempDirPath).init
  }

  "deleteWorkingDirectoryFiles" should "delete all files indexed" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add(tempDirPath).add(Array(f1.pathAsString, f2.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Checkout(tempDirPath).deleteWorkingDirectoryFiles()
    assert(!f1.exists && !f2.exists)
  }

  "createWorkingDirectoryFiles" should "create all files contained in the map" in {
    val f1 = (tempDirPath/"1")
    val f2 = (tempDirPath/"dir"/"2")
    (tempDirPath/".sgit"/"objects"/"d0"/"12345679").createFileIfNotExists(true).overwrite("test")

    val map = Map("1" -> "d012345679") + ("dir/2" -> "d012345679")
    Checkout(tempDirPath).createWorkingDirectoryFiles(map)
    assert(f1.exists && f2.exists && f1.contentAsString == "test" && f2.contentAsString == "test")
  }

  "isThereDiffIndexRepo" should "be true if index != repo" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add(tempDirPath).add(Array(f1.pathAsString, f2.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    f2.overwrite("test2")
    val mapIndex = Index(tempDirPath).getMapFromIndex().getOrElse(Map())
    val diff = Checkout(tempDirPath).isThereDiffIndexRepo(mapIndex)
    assert(diff)
  }

  it should "be false if index == repo" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add(tempDirPath).add(Array(f1.pathAsString, f2.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    val mapIndex = Index(tempDirPath).getMapFromIndex().getOrElse(Map())
    val diff = Checkout(tempDirPath).isThereDiffIndexRepo(mapIndex)
    assert(!diff)
  }

  "isThereLocalChanges" should "be true if index != commit" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add(tempDirPath).add(Array(f1.pathAsString, f2.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    f2.overwrite("test2")
    Add(tempDirPath).add(Array(f2.pathAsString))
    val diff = Checkout(tempDirPath).isThereLocalChanges.getOrElse(false)
    assert(diff)
  }

  it should "be false if index == commit" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add(tempDirPath).add(Array(f1.pathAsString, f2.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    val diff = Checkout(tempDirPath).isThereLocalChanges.getOrElse(false)
    assert(!diff)
  }

  "the checkout command" should "modify the HEAD file to ref the branch" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add(tempDirPath).add(Array(f1.pathAsString, f2.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Branch(tempDirPath).newBranch("test")
    Checkout(tempDirPath).checkout("test")
    val headFile = (tempDirPath/".sgit"/"HEAD")
    assert(headFile.contentAsString == "ref: refs/heads/test")
  }

  it should "pass in detached mode when it co to a tag" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add(tempDirPath).add(Array(f1.pathAsString, f2.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Tag(tempDirPath).newTag("test")
    Checkout(tempDirPath).checkout("test")
    val headFile = (tempDirPath/".sgit"/"HEAD")
    assert(headFile.contentAsString == "ref: refs/detached")
  }

  it should "pass in detached mode when it co to a commit" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add(tempDirPath).add(Array(f1.pathAsString, f2.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    val currBranch =  BranchTool(tempDirPath).getCurrentHeadFile.getOrElse(File("zjjkapej"))
    val sha = currBranch.contentAsString
    Checkout(tempDirPath).checkout(sha)
    val headFile = (tempDirPath/".sgit"/"HEAD")
    assert(headFile.contentAsString == "ref: refs/detached")
  }
}
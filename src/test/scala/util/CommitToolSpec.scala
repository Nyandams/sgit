package util
import command.{Add, Init, Commit}
import java.io
import java.nio.file.Files
import util.CommitTool._
import better.files._
import org.scalatest.{BeforeAndAfterEach, FlatSpec}


class CommitToolSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
  }

  "CommitTool.isThereACommit" should "return true if there is a commit" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    assert(isThereACommit(tempDirPath))
  }

  "CommitTool.isThereACommit" should "return false if there isn't a commit" in {
    assert(!isThereACommit(tempDirPath))
  }

  "CommitTool.getMapBlobCommit" should "return a map of the size of all the files in the commit" in {
    val f1 = (tempDirPath/"1").createFile()
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add.add(tempDirPath, Array(f1.pathAsString, f2.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    val commitSha = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    getMapBlobCommit(tempDirPath, commitSha) match {
      case Left(error) => assert(false)
      case Right(mapCommit) => assert(mapCommit.size == 2)
    }
  }

  it should "return a map containing the element added in the commit" in {
    val f1 = (tempDirPath/"1").createFile()
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add.add(tempDirPath, Array(f1.pathAsString, f2.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    val commitSha = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    getMapBlobCommit(tempDirPath, commitSha) match {
      case Left(error) => assert(false)
      case Right(mapCommit) => assert(mapCommit.contains("1") && mapCommit.contains("dir/2"))
    }
  }

  it should "return an error if the sha1 is incorrect" in {
    val f1 = (tempDirPath/"1").createFile()
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add.add(tempDirPath, Array(f1.pathAsString, f2.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    getMapBlobCommit(tempDirPath, "sdfjmsdj") match {
      case Left(error) => assert(true)
      case Right(mapCommit) => assert(false)
    }
  }

  "CommitTool.getMapFromCommit" should "return a map containing the information of the commit (no parent)" in {
    val f1 = (tempDirPath/"1").createFile()
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add.add(tempDirPath, Array(f1.pathAsString, f2.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    val commitSha = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    getMapFromCommit(tempDirPath, commitSha) match {
      case Left(error) => assert(false)
      case Right(mapCommit) => assert(mapCommit.contains("tree") && mapCommit.contains("msg"))
    }
  }

  it should "return a map containing the information of the commit (with parent)" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add.add(tempDirPath, Array(f2.pathAsString))
    Commit.commit(tempDirPath, "2nd Commit")
    val commitSha = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    getMapFromCommit(tempDirPath, commitSha) match {
      case Left(error) => assert(false)
      case Right(mapCommit) => assert(mapCommit.contains("tree") && mapCommit.contains("msg") && mapCommit.contains("parent"))
    }
  }

  it should "return an error if the sha is invalid" in {
    val f1 = (tempDirPath/"1").createFile()
    val f2 = (tempDirPath/"dir"/"2").createFileIfNotExists(true)
    Add.add(tempDirPath, Array(f1.pathAsString, f2.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    getMapFromCommit(tempDirPath, "khkjhjkh") match {
      case Left(error) => assert(true)
      case Right(mapCommit) => assert(false)
    }
  }


}
package util
import command.Init
import java.io
import java.nio.file.Files
import util.FileTool._
import better.files._
import org.scalatest.{FlatSpec, BeforeAndAfterEach}


class FileToolSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init(tempDirPath).init
  }

  "FileTool.sha1Hash" should "hash correctly a String" in {
    assert(sha1Hash("test") == "a94a8fe5ccb19ba61c4c0873d391e987982fbbd3")
  }

  "FileTool.allFileRepoSet" should "not contains .sgit folder" in {
    val f1 = (tempDirPath/"1").createFileIfNotExists(true)
    val f2 = (tempDirPath/"d1"/"2").createFileIfNotExists(true)
    val allFiles = allFileRepoSet(tempDirPath)
    assert(allFiles.contains(tempDirPath.relativize(f1).toString) && allFiles.contains(tempDirPath.relativize(f2).toString))
  }

  it should "not contains .sgit files" in {
    val allFiles = allFileRepoSet(tempDirPath)
    val sgitFiles = allFiles.filter(src => src.contains(".sgit"))
    assert(sgitFiles.isEmpty)
  }

  "FileTool.isInSgit" should "return true if it's in a sgit repo" in {
    val isSgit = isInSgit(tempDirPath)
    assert(isSgit)
  }

  it should "return false if it's not in a sgit repo" in {
    (tempDirPath/".sgit").delete()
    val isSgit = isInSgit(tempDirPath)
    assert(!isSgit)
  }

  "FileTool.getSgitRec" should "return the repo if we are in a sgit repo" in {
    getSgitRec(tempDirPath) match {
      case Left(error) => assert(false)
      case Right(repo) => assert(repo.exists)
    }
  }

  "FileTool.getSgitRec" should "return an error if we aren't in a git repo" in {
    (tempDirPath/".sgit").delete()
    getSgitRec(tempDirPath) match {
      case Left(error) => assert(true)
      case Right(repo) => assert(false)
    }
  }

}
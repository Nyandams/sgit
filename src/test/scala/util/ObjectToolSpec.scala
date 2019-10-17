package util
import command.Init
import java.io
import java.nio.file.Files
import util.ObjectTool._
import better.files._
import util.FileTool.sha1Hash
import command.Add.add
import org.scalatest.{FlatSpec, BeforeAndAfterEach}


class ObjectToolSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
  }

  "ObjectTool.getFileFromSha" should "return the corresponding object if it exists" in {
    val file = (tempDirPath/"1").createFileIfNotExists(true)
    file.overwrite("test")
    add(tempDirPath, Array(file.pathAsString))
    val sha = sha1Hash("test")
    getFileFromSha(tempDirPath, sha) match {
      case Left(error) => assert(false)
      case Right(file) => assert(file.exists)
    }
  }

  it should "return an error if the object doesn't exists" in {
    val file = (tempDirPath/"1").createFileIfNotExists(true).overwrite("test")
    val sha = file.sha1
    getFileFromSha(tempDirPath, sha) match {
      case Left(error) => assert(true)
      case Right(file) => assert(false)
    }
  }

  "ObjectTool.getFileFromShaIncomplete" should "return the corresponding object if it exists" in {
    val file = (tempDirPath/"1").createFileIfNotExists(true)
    file.overwrite("test")
    add(tempDirPath, Array(file.pathAsString))
    val sha = sha1Hash("test")
    getFileFromShaIncomplete(tempDirPath, sha.substring(0,3)) match {
      case Left(error) => assert(false)
      case Right(file) => assert(file.exists)
    }
  }

  it should "return an error if the object doesn't exists" in {
    val file = (tempDirPath/"1").createFileIfNotExists(true).overwrite("test")
    val sha = file.sha1
    getFileFromShaIncomplete(tempDirPath, sha.substring(0,3)) match {
      case Left(error) => assert(true)
      case Right(file) => assert(false)
    }
  }

}
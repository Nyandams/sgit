package util
import command.Init
import java.io
import java.nio.file.Files
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
    ObjectTool(tempDirPath).getFileFromSha(sha) match {
      case Left(error) => assert(false)
      case Right(file) => assert(file.exists)
    }
  }

  it should "return an error if the object doesn't exists" in {
    val file = (tempDirPath/"1").createFileIfNotExists(true).overwrite("test")
    val sha = file.sha1
    ObjectTool(tempDirPath).getFileFromSha(sha) match {
      case Left(error) => assert(true)
      case Right(file) => assert(false)
    }
  }

  "ObjectTool.getFileFromShaIncomplete" should "return the corresponding object if it exists" in {
    val file = (tempDirPath/"1").createFileIfNotExists(true)
    file.overwrite("test")
    add(tempDirPath, Array(file.pathAsString))
    val sha = sha1Hash("test")
    ObjectTool(tempDirPath).getFileFromShaIncomplete(sha.substring(0,3)) match {
      case Left(error) => assert(false)
      case Right(file) => assert(file.exists)
    }
  }

  it should "return an error if the object doesn't exists" in {
    val file = (tempDirPath/"1").createFileIfNotExists(true).overwrite("test")
    val sha = file.sha1
    ObjectTool(tempDirPath).getFileFromShaIncomplete(sha.substring(0,3)) match {
      case Left(error) => assert(true)
      case Right(file) => assert(false)
    }
  }

  it should "return an error if the sha1 is incorrect" in {
    val file = (tempDirPath/"1").createFileIfNotExists(true)
    file.overwrite("test")
    add(tempDirPath, Array(file.pathAsString))
    ObjectTool(tempDirPath).getFileFromShaIncomplete("sdok") match {
      case Left(error) => assert(true)
      case Right(file) => assert(false)
    }
  }

  it should "return an error if there is multiple file corresponding" in {
    (tempDirPath/"objects"/"d0"/"12556658").createFileIfNotExists(true)
    (tempDirPath/"objects"/"d0"/"1657").createFileIfNotExists(true)
    val sha = "d01"
    ObjectTool(tempDirPath).getFileFromShaIncomplete(sha) match {
      case Left(error) => assert(true)
      case Right(file) => assert(false)
    }
  }

  it should "return an error if the shaCut is too short" in {
    ObjectTool(tempDirPath).getFileFromShaIncomplete("d0") match {
      case Left(error) => assert(true)
      case Right(file) => assert(false)
    }
  }
}
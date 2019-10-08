package command
import java.io
import java.nio.file.Files

import better.files._
import org.scalatest._


class AddSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
    val ex = (tempDirPath/".sgit").exists
  }

  "The add command" should "run" in {
    Add.add(tempDirPath, Array())
  }

  it should "write in index file" in {
    val f = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f.pathAsString))
    val index = tempDirPath/".sgit"/"index"
    assert(index.contentAsString != "")
  }
}
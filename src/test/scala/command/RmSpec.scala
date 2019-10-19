package command
import java.io
import java.nio.file.Files
import better.files._
import org.scalatest.{FlatSpec, BeforeAndAfterEach}


class RmSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init(tempDirPath).init
  }

  "The rm command" should "run" in {
    Rm(tempDirPath).rm(Array())
  }

  it should "delete the file that has been removed" in {
    val f = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f.pathAsString))
    Rm(tempDirPath).rm(Array(f.pathAsString))
    assert(!f.exists)
  }

  it should "delete a line in index" in {
    val f = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f.pathAsString))
    val index = (tempDirPath/".sgit/index")
    val nbLine = index.lineCount
    Rm(tempDirPath).rm(Array(f.pathAsString))
    assert(nbLine == index.lineCount + 1)
  }
  
}
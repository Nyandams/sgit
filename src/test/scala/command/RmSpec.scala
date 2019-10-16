package command
import java.io
import java.nio.file.Files
import java.io.File.separator
import util.FileTool.sha1Hash
import better.files._
import objects.Index._
import org.scalatest.{FlatSpec, FunSpec, Matchers, BeforeAndAfterEach}


class RmSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
  }

  "The rm command" should "run" in {
    Rm.rm(tempDirPath, Array())
  }

  it should "delete the file that has been removed" in {
    val f = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f.pathAsString))
    Rm.rm(tempDirPath, Array(f.pathAsString))
    assert(!f.exists)
  }

  it should "delete a line in index" in {
    val f = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f.pathAsString))
    val index = (tempDirPath/".sgit/index")
    val nbLine = index.lineCount
    Rm.rm(tempDirPath, Array(f.pathAsString))
    assert(nbLine == index.lineCount + 1)
  }
  
}
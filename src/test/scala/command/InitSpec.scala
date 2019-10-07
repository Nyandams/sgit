package command

import java.io
import java.nio.file.Files

import better.files._
import org.scalatest._

class InitSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit ={
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
  }

  "A repository .sgit" should "be initializable" in {
    Init.init(tempDirPath)
  }

  it should "exist in the disk" in {
    Init.init(tempDirPath)
    var sgitDir = tempDirPath/".sgit"
    assert(sgitDir.isDirectory)
  }

  it should "contains a directory objects" in {
    Init.init(tempDirPath)
    var dir = tempDirPath/".sgit"/"objects"
    assert(dir.isDirectory)
  }

  it should "contains a directory refs" in {
    Init.init(tempDirPath)
    var dir = tempDirPath/".sgit"/"refs"
    assert(dir.isDirectory)
  }

  it should "contains a directory refs/tags" in {
    Init.init(tempDirPath)
    var dir = tempDirPath/".sgit"/"refs"/"tags"
    assert(dir.isDirectory)
  }

  it should "contains a directory refs/heads" in {
    Init.init(tempDirPath)
    var dir = tempDirPath/".sgit"/"refs"/"heads"
    assert(dir.isDirectory)
  }

  it should "contains a file HEAD" in {
    Init.init(tempDirPath)
    var file = tempDirPath/".sgit"/"HEAD"
    assert(file.exists)
  }

  it should "contains a file index" in {
    Init.init(tempDirPath)
    var file = tempDirPath/".sgit"/"index"
    assert(file.exists)
  }

  it should "contains a file HEAD that refers to refs/heads/master" in {
    Init.init(tempDirPath)
    val file = tempDirPath/".sgit"/"HEAD"
    var line = file.lines.toList

    assert(line(0) == "ref: refs/heads/master")
  }

}

package command
import java.io
import java.nio.file.Files
import java.io.File.separator
import util.FileTool.sha1Hash
import better.files._
import objects.Index._
import org.scalatest.{FlatSpec, FunSpec, Matchers, BeforeAndAfterEach}


class AddSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
  }

  "The add command" should "run" in {
    Add.add(tempDirPath, Array())
  }

  it should "write in index file the good line" in {
    val f = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f.pathAsString))
    val src = f.pathAsString.replace(tempDirPath.pathAsString + separator, "")
    val index = tempDirPath/".sgit"/"index"

    assert(index.contentAsString == ("da39a3ee5e6b4b0d3255bfef95601890afd80709 " + src + "\n"))
  }

  it should "write in index file multiple lines for multiple files added" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    val src1 = f1.pathAsString.replace(tempDirPath.pathAsString + separator, "")

    val f2 = (tempDirPath/"d1"/"2").createFileIfNotExists(true).appendText("this is file 2")
    Add.add(tempDirPath, Array(f2.pathAsString))
    val src2 = f2.pathAsString.replace(tempDirPath.pathAsString + separator, "")

    val index = tempDirPath/".sgit"/"index"

      assert(index.contentAsString.contains("4b12046facfee0eca1fa3d1910c4c5baad3e660f " + src2 + "\n") &&
             index.contentAsString.contains("da39a3ee5e6b4b0d3255bfef95601890afd80709 " + src1 + "\n"))
  }

  it should "create a blob in objects with the sha-1 of the content as name" in {
    val f = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f.pathAsString))
    val sha = sha1Hash(f.contentAsString)
    assert((tempDirPath/".sgit"/"objects"/sha.substring(0,2)/sha.substring(2)).exists())
  }

  it should "create a blob with the same content as the original file" in {
    val f = (tempDirPath/"1").createFile()
    f.appendText("this is file 2")
    Add.add(tempDirPath, Array(f.pathAsString))
    val sha = sha1Hash(f.contentAsString)
    val blob = tempDirPath/".sgit"/"objects"/sha.substring(0,2)/sha.substring(2)
    assert(blob.contentAsString == f.contentAsString)
  }

  it should "modify the sha1 of a file added that got modified" in {
    val index = (tempDirPath/".sgit/index")
    val f = (tempDirPath/"1").createFile()
    f.appendText("this is file 2")
    val shaIndex = sha1Hash(index.contentAsString)
    Add.add(tempDirPath, Array(f.pathAsString))
    f.appendText("this is file 3")
    Add.add(tempDirPath, Array(f.pathAsString))
    val newShaIndex = sha1Hash(index.contentAsString)

    assert(shaIndex != newShaIndex)
  }

}
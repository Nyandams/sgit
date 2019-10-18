package command
import java.io
import java.nio.file.Files
import better.files._
import org.scalatest.{FlatSpec, BeforeAndAfterEach}


class TagSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
  }

  "The newTag command" should "create a tag in .sgit/refs/tags with the right content in it" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    Tag.newTag(tempDirPath, "testTag")
    val sha = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    val tagFile = (tempDirPath/".sgit"/"refs"/"tags"/"testTag")
    assert(tagFile.exists && tagFile.contentAsString == sha)
  }

  it should "not create a tag if there is no commit" in {
    Tag.newTag(tempDirPath, "testTag")
    val tagFile = (tempDirPath/".sgit"/"refs"/"tags"/"testTag")
    assert(!tagFile.exists)
  }

  it should "not create a tag if it already exists" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "1st Commit")
    Tag.newTag(tempDirPath, "testTag")
    val tagFile1Content = (tempDirPath/".sgit"/"refs"/"tags"/"testTag").contentAsString

    val f2 = (tempDirPath/"2").createFile()
    Add.add(tempDirPath, Array(f2.pathAsString))
    Commit.commit(tempDirPath, "2nd Commit")
    Tag.newTag(tempDirPath, "testTag")
    val tagFile2Content = (tempDirPath/".sgit"/"refs"/"tags"/"testTag").contentAsString
    assert(tagFile2Content == tagFile1Content)
  }

}
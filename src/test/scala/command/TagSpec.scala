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
    Init(tempDirPath).init
  }

  "The newTag command" should "create a tag in .sgit/refs/tags with the right content in it" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Tag(tempDirPath).newTag("testTag")
    val sha = (tempDirPath/".sgit"/"refs"/"heads"/"master").contentAsString
    val tagFile = (tempDirPath/".sgit"/"refs"/"tags"/"testTag")
    assert(tagFile.exists && tagFile.contentAsString == sha)
  }

  it should "not create a tag if there is no commit" in {
    Tag(tempDirPath).newTag("testTag")
    val tagFile = (tempDirPath/".sgit"/"refs"/"tags"/"testTag")
    assert(!tagFile.exists)
  }

  it should "not create a tag if it already exists" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Tag(tempDirPath).newTag("testTag")
    val tagFile1Content = (tempDirPath/".sgit"/"refs"/"tags"/"testTag").contentAsString

    val f2 = (tempDirPath/"2").createFile()
    Add(tempDirPath).add(Array(f2.pathAsString))
    Commit(tempDirPath).commit("2nd Commit")
    Tag(tempDirPath).newTag("testTag")
    val tagFile2Content = (tempDirPath/".sgit"/"refs"/"tags"/"testTag").contentAsString
    assert(tagFile2Content == tagFile1Content)
  }

  "the showTag command" should "return a number of line corresponding to the number of tag" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Tag(tempDirPath).newTag("testTag")
    Tag(tempDirPath).newTag("tag2")
    val printLength = Tag(tempDirPath).showTags.split("\n").length
    assert(printLength == 2)
  }

  "the showTag command" should "return all the tag created" in {
    val f1 = (tempDirPath/"1").createFile()
    Add(tempDirPath).add(Array(f1.pathAsString))
    Commit(tempDirPath).commit("1st Commit")
    Tag(tempDirPath).newTag("testTag")
    Tag(tempDirPath).newTag("tag2")
    val printArray = Tag(tempDirPath).showTags.split("\n")
    assert(printArray.contains("tag2") && printArray.contains("testTag"))
  }

}
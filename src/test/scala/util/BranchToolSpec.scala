package util
import command.Init
import java.io
import java.nio.file.Files
import better.files._
import org.scalatest.{FlatSpec, BeforeAndAfterEach}


class BranchToolSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
  }

  "BranchTool.getHeadFilePath" should "run" in {
    BranchTool(tempDirPath).getCurrentHeadFile match {
      case Left(error) =>
      case Right(head) =>
    }
  }

  it should "return an error if there is no HEAD file" in {
    (tempDirPath/".sgit"/"HEAD").delete()
    BranchTool(tempDirPath).getCurrentHeadFile match {
      case Left(error) => assert(true)
      case Right(head) => assert(false)
    }
  }

  it should "return the path of the ref of the HEAD" in {
    BranchTool(tempDirPath).getRelativeCurrentHeadFilePath match {
      case Left(error) => assert(false)
      case Right(headRef) => assert(headRef.nonEmpty)
    }
  }

  "BranchTool.getCurrentHeadFile" should "run" in {
    BranchTool(tempDirPath).getCurrentHeadFile match {
      case Left(error) =>
      case Right(head) =>
    }
  }

  it should "return an error if there is no HEAD file" in {
    (tempDirPath/".sgit"/"HEAD").delete()
    BranchTool(tempDirPath).getCurrentHeadFile match {
      case Left(error) => assert(true)
      case Right(head) => assert(false)
    }
  }

  it should "return a file corresponding to the current branch" in {
    BranchTool(tempDirPath).getCurrentHeadFile match {
      case Left(error) => assert(false)
      case Right(branch) => assert(branch.exists)
    }
  }

}
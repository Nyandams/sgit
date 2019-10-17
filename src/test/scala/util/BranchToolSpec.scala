package util
import command.Init
import java.io
import java.nio.file.Files
import java.io.File.separator
import util.FileTool.sha1Hash
import better.files._
import objects.Index._
import org.scalatest.{FlatSpec, FunSpec, Matchers, BeforeAndAfterEach}


class BranchToolSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
  }

  "BranchTool.getHeadFilePath" should "run" in {
    BranchTool.getHeadFilePath(tempDirPath) match {
      case Left(error) =>
      case Right(head) =>
    }
  }

  it should "return an error if there is no HEAD file" in {
    (tempDirPath/".sgit"/"HEAD").delete()
    BranchTool.getHeadFilePath(tempDirPath) match {
      case Left(error) => assert(true)
      case Right(head) => assert(false)
    }
  }

  it should "return the path of the ref of the HEAD" in {
    BranchTool.getHeadFilePath(tempDirPath) match {
      case Left(error) => assert(false)
      case Right(headRef) => assert(headRef.nonEmpty)
    }
  }

  "BranchTool.getCurrentBranch" should "run" in {
    BranchTool.getCurrentBranch(tempDirPath) match {
      case Left(error) =>
      case Right(head) =>
    }
  }

  it should "return an error if there is no HEAD file" in {
    (tempDirPath/".sgit"/"HEAD").delete()
    BranchTool.getCurrentBranch(tempDirPath) match {
      case Left(error) => assert(true)
      case Right(head) => assert(false)
    }
  }

  it should "return a file corresponding to the current branch" in {
    BranchTool.getCurrentBranch(tempDirPath) match {
      case Left(error) => assert(false)
      case Right(branch) => assert(branch.exists)
    }
  }

}
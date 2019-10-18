package command
import java.io
import java.nio.file.Files

import better.files._
import objects.Index.getMapFromIndex
import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.BranchTool.getCurrentBranch
import util.CommitTool.getMapBlobCommit


class StatusSpec extends FlatSpec with BeforeAndAfterEach {
  var tempDir: io.File = Files.createTempDirectory("testRepo").toFile
  var tempDirPath = File("")

  override def beforeEach(): Unit = {
    tempDir = Files.createTempDirectory("testRepo").toFile
    tempDirPath = File(tempDir.getCanonicalPath)
    Init.init(tempDirPath)
  }

  "The Status command" should "get files added that need to be commited" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "commit")
    val f2 = (tempDirPath/"2").createFile()
    Add.add(tempDirPath, Array(f2.pathAsString))
    getMapFromIndex(tempDirPath) match {
      case Right(mapIndex) =>
        val currentBranch =  getCurrentBranch(tempDirPath).getOrElse(File("3"))
        val sha1Commit = currentBranch.contentAsString
        val mapCommit = getMapBlobCommit(tempDirPath, sha1Commit).getOrElse(Map())
        val changes = Status.getChangesToCommit(tempDirPath,mapIndex, mapCommit, tempDirPath)
        assert(changes.nonEmpty)
      case Left(error) => assert(false)
    }
  }

  it should "not get files that need to be commited when not needed" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    Commit.commit(tempDirPath, "commit")
    getMapFromIndex(tempDirPath) match {
      case Right(mapIndex) =>
        val currentBranch =  getCurrentBranch(tempDirPath).getOrElse(File("3"))
        val sha1Commit = currentBranch.contentAsString
        val mapCommit = getMapBlobCommit(tempDirPath, sha1Commit).getOrElse(Map())
        val changes = Status.getChangesToCommit(tempDirPath,mapIndex, mapCommit, tempDirPath)
        assert(changes.isEmpty)
      case Left(error) => assert(false)
    }
  }

  it should "get nothing is there is no modification in the working directory that need to be added" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    val f2 = (tempDirPath/"2").createFile()
    Add.add(tempDirPath, Array(f2.pathAsString))
    getMapFromIndex(tempDirPath) match {
      case Right(mapIndex) =>

        val changes = Status.getNotStagedChanges(tempDirPath,mapIndex, tempDirPath)
        assert(changes.isEmpty)
      case Left(error) => assert(false)
    }
  }

  it should "get files modified that need to be added" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    val f2 = (tempDirPath/"2").createFile()
    Add.add(tempDirPath, Array(f2.pathAsString))
    f1.overwrite("modif")
    getMapFromIndex(tempDirPath) match {
      case Right(mapIndex) =>

        val changes = Status.getNotStagedChanges(tempDirPath,mapIndex, tempDirPath)
        assert(changes.nonEmpty)
      case Left(error) => assert(false)
    }
  }

  it should "get files deleted that need to be rm" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    val f2 = (tempDirPath/"2").createFile()
    Add.add(tempDirPath, Array(f2.pathAsString))
    f1.delete()
    getMapFromIndex(tempDirPath) match {
      case Right(mapIndex) =>

        val changes = Status.getNotStagedChanges(tempDirPath,mapIndex, tempDirPath)
        assert(changes.nonEmpty)
      case Left(error) => assert(false)
    }
  }

  it should "get untracked Files" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    val f2 = (tempDirPath/"2").createFile()
    Add.add(tempDirPath, Array(f2.pathAsString))
    (tempDirPath/"3").createFile()
    getMapFromIndex(tempDirPath) match {
      case Right(mapIndex) =>
        val changes = Status.getUntrackedFiles(tempDirPath,mapIndex.keySet, tempDirPath)
        assert(changes.nonEmpty)
      case Left(error) => assert(false)
    }
  }

  it should "get nothing is there is no untracked Files" in {
    val f1 = (tempDirPath/"1").createFile()
    Add.add(tempDirPath, Array(f1.pathAsString))
    val f2 = (tempDirPath/"2").createFile()
    Add.add(tempDirPath, Array(f2.pathAsString))
    getMapFromIndex(tempDirPath) match {
      case Right(mapIndex) =>
        val changes = Status.getUntrackedFiles(tempDirPath,mapIndex.keySet, tempDirPath)
        assert(changes.isEmpty)
      case Left(error) => assert(false)
    }
  }

}
package command

import better.files.File
import util.CommitTool
import java.io.File.separator

import objects.Index
import util.BranchTool

import annotation.tailrec
import util.ObjectTool
import util.FileTool.{allFileRepoSet, sha1Hash}

case class Checkout(repo: File) {

  /**
    * @param coElement a branch name, a tag or a commit
    */
  def checkout(coElement: String): String = {
    isThereLocalChanges match {
      case Left(error) => error
      case Right(isThereDiff) =>
        if (isThereDiff) {
          "You have local changes; cannot switch branches."
        } else {
          updateHeadCheckout(coElement) match {
            case Left(error) => error
            case Right((shaCommit, toPrint)) =>
              deleteWorkingDirectoryFiles()
              CommitTool(repo).getMapBlobCommit(shaCommit) match {
                case Left(error) => error
                case Right(mapCommit) =>
                  val index = Index(repo)
                  index.updateIndex(mapCommit)
                  createWorkingDirectoryFiles(mapCommit)
                  toPrint
              }
          }
        }
    }
  }

  /**
    * Update the HEAD and return the sha of the commit corresponding
    * @param checkoutParam
    * @return sha of the new current commit
    */
  private def updateHeadCheckout(
      checkoutParam: String
  ): Either[String, (String, String)] = {
    val branchTool = BranchTool(repo)
    val possibleBranch = branchTool.getBranchFile(checkoutParam)
    val possibleTag = branchTool.getTagFile(checkoutParam)
    var shaCommit = ""

    if (possibleBranch.exists) {
      branchTool.updateHeadRefBranch(checkoutParam)
      Right(
        (
          possibleBranch.contentAsString,
          s"Switched to branch '${checkoutParam}'"
        )
      )
    } else if (possibleTag.exists) {
      shaCommit = possibleTag.contentAsString
      branchTool.updateHeadRefDetached(shaCommit)
      Right((shaCommit, s"Switched to tag '${checkoutParam}'"))
    } else {
      ObjectTool(repo).getFileFromShaIncomplete(checkoutParam) match {
        case Left(error) => Left(error)
        case Right(fileCommit) =>
          shaCommit = fileCommit.parent.name + fileCommit.name
          branchTool.updateHeadRefDetached(shaCommit)
          val toPrint =
            s"Note: switching to '${shaCommit}'.\n\n You are in 'detached HEAD' state. You can look around, make experimental\nchanges and commit them, and you can discard any commits you make in this\nstate without impacting any branches by switching back to a branch."
          Right(shaCommit, toPrint)
      }
    }
  }

  def isThereLocalChanges: Either[String, Boolean] = {
    val index = Index(repo)
    index.getMapFromIndex() match {
      case Left(error) => Left(error)
      case Right(mapIndex) =>
        BranchTool(repo).getCurrentHeadFile match {
          case Left(error) => Left(error)
          case Right(currentBranch) =>
            val mapCommit =
              CommitTool(repo).getMapBlobCommit(currentBranch.contentAsString)
                .getOrElse(Map())
            val isDiffCommitIndex = if (mapCommit == mapIndex) false else true
            val isDiffIndexRepo = isThereDiffIndexRepo(mapIndex)
            Right(isDiffCommitIndex || isDiffIndexRepo)
        }
    }
  }

  def isThereDiffIndexRepo(
      mapIndex: Map[String, String]
  ): Boolean = {
    val indexedFiles = mapIndex.keySet
    val allFileRepo = allFileRepoSet(repo)

    val deletedFiles = indexedFiles.diff(allFileRepo)

    val existingIndexedFiles = indexedFiles.diff(deletedFiles)
    val modifiedFiles = existingIndexedFiles.filter(
      f => mapIndex(f) != sha1Hash((repo / f).contentAsString)
    )

    (deletedFiles.nonEmpty || modifiedFiles.nonEmpty)
  }

  def createWorkingDirectoryFiles(
      mapCommit: Map[String, String]
  ): Unit = {
    @tailrec
    def createFile(mapCommit: Map[String, String]): Unit = {
      if (mapCommit.nonEmpty) {
        val fileToCreate = mapCommit.head
        ObjectTool(repo).getFileFromShaIncomplete(fileToCreate._2) match {
          case Left(error) =>
          case Right(file) =>
            val fileCreated = (repo / fileToCreate._1)
              .createFileIfNotExists(createParents = true)
            fileCreated.overwrite(file.contentAsString)
        }
        createFile(mapCommit.tail)
      }
    }
    createFile(mapCommit)
  }

  def deleteWorkingDirectoryFiles(): Unit = {
    @tailrec
    def deleteFiles(listFiles: Iterable[File]): Unit = {
      if (listFiles.nonEmpty) {
        val fileToDelete = listFiles.head
        if (fileToDelete.exists) {
          fileToDelete.delete()
        }
        deleteFiles(listFiles.tail)
      }
    }
    val index = Index(repo)
    index.getMapFromIndex() match {
      case Left(error) =>
      case Right(mapIndex) =>
        val filesToDelete = mapIndex.keySet.map(src => (repo / src))
        deleteFiles(filesToDelete)
    }
  }
}

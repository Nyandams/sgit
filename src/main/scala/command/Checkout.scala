package command

import better.files.File
import objects.Index.{getMapFromIndex, updateIndex}
import util.CommitTool.getMapBlobCommit
import java.io.File.separator
import annotation.tailrec
import util.ObjectTool.getFileFromShaIncomplete
import util.BranchTool.getCurrentBranch
import util.FileTool.{allFileRepoSet, sha1Hash}

object Checkout {

  /**
    *
    * @param repo
    * @param coElement a branch name, a tag or a commit
    */
  def checkout(repo: File, coElement: String): String = {
    isThereLocalChanges(repo) match {
      case Left(error) => error
      case Right(isThereDiff) =>
        if (isThereDiff) {
          "You have local changes; cannot switch branches."
        } else {
          val possibleBranch = (repo / ".sgit" / "refs" / "heads" / coElement)
          val possibleTag = (repo / ".sgit" / "refs" / "tags" / coElement)
          val headFile = (repo / ".sgit" / "HEAD")
          var toPrint = ""
          var shaCommit = ""
          if (possibleBranch.exists) {

            headFile.overwrite(
              "ref: refs" + separator + "heads" + separator + coElement
            )
            shaCommit = possibleBranch.contentAsString
            toPrint += s"Switched to branch '${coElement}'"
          } else if (possibleTag.exists) {
            shaCommit = possibleTag.contentAsString
            toPrint += s"Switched to tag '${coElement}'"
            val detached = (repo / ".sgit" / "refs" / "detached")
              .createFileIfNotExists(createParents = true)
            detached.overwrite(shaCommit)
            headFile.overwrite("ref: refs" + separator + detached.name)
          } else {
            getFileFromShaIncomplete(repo, coElement) match {
              case Left(error) => error
              case Right(fileCommit) =>
                shaCommit = fileCommit.parent.name + fileCommit.name
                toPrint +=
                  s"Note: switching to '${shaCommit}'.\n\n You are in 'detached HEAD' state. You can look around, make experimental\nchanges and commit them, and you can discard any commits you make in this\nstate without impacting any branches by switching back to a branch."
                val detached = (repo / ".sgit" / "refs" / "detached")
                  .createFileIfNotExists(createParents = true)
                detached.overwrite(shaCommit)
                headFile.overwrite("ref: refs" + separator + detached.name)
            }
          }
          deleteWorkingDirectoryFiles(repo)
          getMapBlobCommit(repo, shaCommit) match {
            case Left(error) => error
            case Right(mapCommit) =>
              updateIndex(repo, mapCommit)
              createWorkingDirectoryFiles(repo, mapCommit)
              toPrint
          }
        }
    }
  }

  def isThereLocalChanges(repo: File): Either[String, Boolean] = {
    getMapFromIndex(repo) match {
      case Left(error) => Left(error)
      case Right(mapIndex) =>
        getCurrentBranch(repo) match {
          case Left(error) => Left(error)
          case Right(currentBranch) =>
            val mapCommit =
              getMapBlobCommit(repo, currentBranch.contentAsString)
                .getOrElse(Map())
            val isDiffCommitIndex = if (mapCommit == mapIndex) false else true
            val isDiffIndexRepo = isThereDiffIndexRepo(repo, mapIndex)
            Right(isDiffCommitIndex || isDiffIndexRepo)
        }
    }
  }

  def isThereDiffIndexRepo(
      repo: File,
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
      repo: File,
      mapCommit: Map[String, String]
  ): Unit = {
    @tailrec
    def createFile(repo: File, mapCommit: Map[String, String]): Unit = {
      if (mapCommit.nonEmpty) {
        val fileToCreate = mapCommit.head
        getFileFromShaIncomplete(repo, fileToCreate._2) match {
          case Left(error) =>
          case Right(file) =>
            val fileCreated = (repo / fileToCreate._1)
              .createFileIfNotExists(createParents = true)
            fileCreated.overwrite(file.contentAsString)
        }
        createFile(repo, mapCommit.tail)
      }
    }

    createFile(repo, mapCommit)
  }

  def deleteWorkingDirectoryFiles(repo: File): Unit = {
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

    getMapFromIndex(repo) match {
      case Left(error) => ???
      case Right(mapIndex) =>
        val filesToDelete = mapIndex.keySet.map(src => (repo / src))
        deleteFiles(filesToDelete)
    }
  }
}

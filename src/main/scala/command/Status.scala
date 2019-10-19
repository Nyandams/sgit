package command

import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED}
import better.files._
import objects.Index
import util.FileTool.{sha1Hash, getUserDirectory, allFileRepoSet}
import util.BranchTool
import util.CommitTool

object Status {

  /**
    * Show untracked files, Modified Files and deleted files
    *
    * @param repo
    */
  def status(repo: File, userDir: File = getUserDirectory): String = {
    var toPrint = ""
    val index = Index(repo)
    index.getMapFromIndex match {
      case Right(mapIndex) =>
        val keys = mapIndex.keySet
        val notStagedChanges = getNotStagedChanges(repo, mapIndex, userDir)
        val untrackedFiles = getUntrackedFiles(repo, keys, userDir)

        BranchTool(repo).getCurrentHeadFile match {
          case Right(currentBranch) =>
            toPrint += s"On branch ${currentBranch.name}\n\n"
            var changesToCommit = ""
            if (currentBranch.isEmpty) {
              toPrint += "No commits yet\n\n"
              changesToCommit =
                getChangesToCommit(repo, mapIndex, Map(), userDir)
            } else {
              val sha1Commit = currentBranch.contentAsString
              val mapCommit =
                CommitTool(repo).getMapBlobCommit(sha1Commit).getOrElse(Map())
              changesToCommit =
                getChangesToCommit(repo, mapIndex, mapCommit, userDir)
            }
            if (changesToCommit.nonEmpty) toPrint += (changesToCommit + "\n")
            if (notStagedChanges.nonEmpty) toPrint += (notStagedChanges + "\n")
            if (untrackedFiles.nonEmpty) toPrint += (untrackedFiles + "\n")

            if (changesToCommit.isEmpty && notStagedChanges.nonEmpty)
              toPrint += "nothing added to commit but untracked files present (use \"git add\" to track)\n"

          case Left(error) => toPrint += (error + "\n")
        }

    }
    toPrint
  }

  def getUntrackedFiles(
      repo: File,
      indexSet: Set[String],
      userDir: File
  ): String = {
    val allFileRepo = allFileRepoSet(repo)

    val untrackedList = allFileRepo.diff(indexSet)
    val untrackedFilesStringArray = untrackedList.map(
      src => s"\t${userDir.relativize(repo / src)}"
    ) mkString "\n"

    if (untrackedFilesStringArray.nonEmpty) {
      return s"Untracked files:\n  (use sgit add <file>... to include in what will be committed)\n${RED}${untrackedFilesStringArray}${RESET}"
    } else {
      return ""
    }
  }

  def getNotStagedChanges(
      repo: File,
      mapIndex: Map[String, String],
      userDir: File
  ): String = {

    val indexedFiles = mapIndex.keySet
    val allFileRepo = allFileRepoSet(repo)

    val deletedFiles = indexedFiles.diff(allFileRepo)
    val deletedFilesStringArray = deletedFiles.map(
      src => s"\tdeleted:    ${userDir.relativize(repo / src)}"
    ) mkString "\n"

    val existingIndexedFiles = indexedFiles.diff(deletedFiles)
    val modifiedFiles = existingIndexedFiles.filter(
      f => mapIndex(f) != sha1Hash((repo / f).contentAsString)
    )
    val modifiedFilesStringArray = modifiedFiles.map(
      src => s"\tmodified:   ${userDir.relativize(repo / src)}"
    ) mkString "\n"

    if (deletedFilesStringArray.nonEmpty || modifiedFilesStringArray.nonEmpty) {
      return s"Changes not staged for commit:\n  (use sgit add/rm <file>... to update what will be committed)\n${RED}${modifiedFilesStringArray}\n${deletedFilesStringArray}${RESET}"
    } else {
      return ""
    }
  }

  def getStagedAddition(
      repo: File,
      mapIndex: Map[String, String],
      mapCommit: Map[String, String],
      userDir: File
  ): String = {
    val newAdditions = mapIndex.keySet.diff(mapCommit.keySet)
    newAdditions.map(src => s"\tnew file:   ${userDir.relativize(repo / src)}") mkString "\n"
  }

  def getStagedDeletion(
      repo: File,
      mapIndex: Map[String, String],
      mapCommit: Map[String, String],
      userDir: File
  ): String = {
    val newDeletions = mapCommit.keySet.diff(mapIndex.keySet)
    newDeletions.map(src => s"\tdeleted:    ${userDir.relativize(repo / src)}") mkString "\n"
  }

  def getStagedModification(
      repo: File,
      mapIndex: Map[String, String],
      mapCommit: Map[String, String],
      userDir: File
  ): String = {
    val intersect = mapIndex.keySet.intersect(mapCommit.keySet)
    val newModifiedFiles = intersect.filter(f => mapIndex(f) != mapCommit(f))
    newModifiedFiles.map(
      src => s"\tmodified:   ${userDir.relativize(repo / src)}"
    ) mkString "\n"
  }

  def getChangesToCommit(
      repo: File,
      mapIndex: Map[String, String],
      mapCommit: Map[String, String],
      userDir: File
  ): String = {
    val stagedAddition = getStagedAddition(repo, mapIndex, mapCommit, userDir)
    val stagedModification =
      getStagedModification(repo, mapIndex, mapCommit, userDir)
    val stagedDeletion = getStagedDeletion(repo, mapIndex, mapCommit, userDir)

    if (stagedAddition.nonEmpty || stagedModification.nonEmpty || stagedDeletion.nonEmpty) {
      s"Changes to be comitted:\n ${GREEN}${stagedAddition}\n${stagedModification}\n${stagedDeletion}${RESET}"
    } else {
      ""
    }
  }

}

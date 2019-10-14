package command

import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED}
import better.files._
import objects.Index.getMapFromIndex
import util.FileTool.{sha1Hash, getUserDirectory, allFileRepoSet}
import util.BranchTool.{getCurrentBranch}
import util.CommitUtil.getMapBlobCommit

object Status {
  /**
   * Show untracked files, Modified Files and deleted files
   *
   * @param repo
   */
  def status(repo: File, userDir: File = getUserDirectory): Unit = {
    getCurrentBranch(repo) match {
      case Left(currentBranch) =>
        println(s"On branch ${currentBranch.name}\n")
        if (currentBranch.isEmpty) println("No commits yet\n")
        else {
          val sha1Commit = currentBranch.contentAsString
          getMapBlobCommit(repo, sha1Commit) match {
            case Left(map) => println(map)
            case Right(error) => println(error)
          }
        }

        getMapFromIndex(repo) match {
          case Left(mapIndex) =>

            val keys = mapIndex.keySet

            val changesToCommit = getChangesToCommit(repo, keys, userDir)
            val notStagedChanges = getNotStagedChanges(repo, mapIndex, userDir)
            val untrackedFiles = getUntrackedFiles(repo, keys, userDir)

            if (changesToCommit.nonEmpty) println(changesToCommit)
            if (notStagedChanges.nonEmpty) println(notStagedChanges)
            if(untrackedFiles.nonEmpty) println(untrackedFiles)

            if(changesToCommit.isEmpty && notStagedChanges.nonEmpty) println("nothing added to commit but untracked files present (use \"git add\" to track)")
          case Right(error) => println(error)

      case Right(error) => println(error)
    }

    }
  }

  def getUntrackedFiles(repo: File, indexSet: Set[String], userDir: File): String = {
    val allFileRepo = allFileRepoSet(repo)

    val untrackedList = allFileRepo.diff(indexSet)
    val untrackedFilesStringArray = untrackedList.map(src => s"\t${  userDir.relativize(repo/src)  }") mkString "\n"

    if(untrackedFilesStringArray.nonEmpty){
      return s"Untracked files:\n  (use sgit add <file>... to include in what will be committed)\n${RED}${untrackedFilesStringArray}${RESET}"
    } else {
      return ""
    }
  }

  def getNotStagedChanges(repo: File, mapIndex: Map[String, String], userDir: File): String = {

    val indexedFiles = mapIndex.keySet
    val allFileRepo = allFileRepoSet(repo)

    val deletedFiles = indexedFiles.diff(allFileRepo)
    val deletedFilesStringArray = deletedFiles.map(src => s"\tdeleted:    ${ userDir.relativize(repo/src) }") mkString "\n"

    val existingIndexedFiles = indexedFiles.diff(deletedFiles)
    val modifiedFiles = existingIndexedFiles.filter(f => mapIndex(f) != sha1Hash((repo/f).contentAsString))
    val modifiedFilesStringArray = modifiedFiles.map(src => s"\tmodified:   ${ userDir.relativize(repo/src) }") mkString "\n"

    if(deletedFilesStringArray.nonEmpty || modifiedFilesStringArray.nonEmpty){
      return s"Changes not staged for commit:\n  (use sgit add/rm <file>... to update what will be committed)\n${RED}${modifiedFilesStringArray}\n${deletedFilesStringArray}${RESET}"
    } else {
      return ""
    }
  }

  def getChangesToCommit(repo: File, indexSet: Set[String], userDir: File): String = {
    return ""
  }

}

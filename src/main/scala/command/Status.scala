package command

import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED}
import better.files._
import objects.Index.getMapFromIndex
import util.FileTool.sha1Hash

object Status {
  /**
   * Show untracked files, Modified Files and deleted files
   *
   * @param repo
   */
  def status(repo: File): Unit = {
    getMapFromIndex(repo) match {
      case Left(mapIndex) =>
        val keys = mapIndex.keySet

        print(getChangesToCommit(repo, keys))
        print(getNotStagedChanges(repo, mapIndex))
        print(getUntrackedFiles(repo, keys))
      case Right(error) => println(error)
    }
  }

  def getUntrackedFiles(repo: File, indexSet: Set[String]): String = {
    val allFileRepoSet =
      repo.listRecursively
      .toSet.filter(f => f.isRegularFile)
      .filter(f => !f.pathAsString.contains(".sgit"))
      .map(f => repo.relativize(f).toString)

    val untrackedList = allFileRepoSet.diff(indexSet)
    val untrackedFilesStringArray = untrackedList.map(src => s"\t${src}") mkString "\n"

    if(untrackedFilesStringArray.nonEmpty){
      return s"Untracked files:\n  (use sgit add <file>... to include in what will be committed)\n${RED}${untrackedFilesStringArray}${RESET}"
    } else {
      return ""
    }
  }

  def getNotStagedChanges(repo: File, mapIndex: Map[String, String]): String = {

    val indexedFiles = mapIndex.keySet
    val allFileRepoSet =
      repo.listRecursively
        .toSet.filter(f => f.isRegularFile)
        .filter(f => !f.pathAsString.contains(".sgit"))
        .map(f => repo.relativize(f).toString)

    val deletedFiles = indexedFiles.diff(allFileRepoSet)
    val deletedFilesStringArray = deletedFiles.map(src => s"\tdeleted:    ${src}") mkString "\n"

    val existingIndexedFiles = indexedFiles.diff(deletedFiles)
    println(mapIndex)
    //println(existingIndexedFiles.map(f => sha1Hash((repo/f).contentAsString)))
    val modifiedFiles = existingIndexedFiles.filter(f => mapIndex(f) != sha1Hash((repo/f).contentAsString))
    val modifiedFilesStringArray = modifiedFiles.map(src => s"\tmodified:   ${src}") mkString "\n"

    if(deletedFilesStringArray.nonEmpty || modifiedFilesStringArray.nonEmpty){
      return s"Changes not staged for commit:\n  (use sgit add/rm <file>... to update what will be committed)\n${RED}${modifiedFilesStringArray}\n${deletedFilesStringArray}${RESET}\n"
    } else {
      return ""
    }
  }

  def getChangesToCommit(repo: File, indexSet: Set[String]): String = {
    return ""
  }

}

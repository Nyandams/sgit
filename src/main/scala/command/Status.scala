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

        showModifiedFiles(repo, mapIndex)
        showDeletedFiles(repo, keys)
        showUntrackedFiles(repo, keys)
      case Right(error) => println(error)
    }
  }

  def showUntrackedFiles(repo: File, indexSet: Set[String]): Unit = {
    val allFileRepoSet =
      repo.listRecursively
      .toSet.filter(f => f.isRegularFile)
      .filter(f => !f.pathAsString.contains(".sgit"))
      .map(f => repo.relativize(f).toString)

    val untrackedList = allFileRepoSet.diff(indexSet)
    val untrackedFilesStringArray = untrackedList.map(src => s"\t${src}") mkString "\n"

    println(untrackedList)
    println(s"Untracked files:\n  (use git add <file>... to include in what will be committed)")

    print(s"${RED}")
    println(untrackedFilesStringArray)
    print(s"${RESET}")

  }

  def showModifiedFiles(repo: File, mapIndex: Map[String, String]): Unit = {

  }

  def showDeletedFiles(repo: File, indexSet: Set[String]): Unit = {

  }

}

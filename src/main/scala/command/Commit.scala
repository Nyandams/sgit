package command

import util.FileTool._
import better.files._
import objects.Index._
import java.util.regex.Pattern
import java.io.File.separator
import annotation.tailrec

object Commit {

  def commit(repo: File, message: String): Unit = {
    getMapFromIndex(repo) match {
      case Left(mapIndex) =>
        val keys = mapIndex.keySet
        val headFile = (repo / ".sgit" / "HEAD").contentAsString.split(" ")(1)
        val currentBranch = (repo / ".sgit" / headFile).createFileIfNotExists()

        if(keys.nonEmpty) {
          val separator = Pattern.quote(System.getProperty("file.separator"))
          val listSorted = keys.toList.map(src => src.split(separator).toList).sortBy(f => f.length).reverse
          if (listSorted.nonEmpty) {
            val sizeMax = listSorted.head.length
            val treeCommit = tree(srcs = listSorted, size = sizeMax, mapIndex = mapIndex, repo = repo)

            val lastCommit = currentBranch.contentAsString

            if (lastCommit.isEmpty) {
              val contentCommit = s"tree ${treeCommit}\n\n${message}"
              val shaCommit = sha1Hash(contentCommit)
              getFileSubDir(repo, shaCommit).createFileIfNotExists(createParents = true).overwrite(contentCommit)
              currentBranch.overwrite(shaCommit)
              println(s"[${currentBranch.name} (root-commit) ${shaCommit.slice(0,8)}] ${message}")
            } else {
              getMapFromCommit(repo, lastCommit) match {
                case Left(mapCommit) =>
                  if (treeCommit == mapCommit("tree")) {
                    println(s"On branch ${currentBranch.name}\nNothing to commit, working tree clean")
                  } else {
                    val contentCommit = s"tree ${treeCommit}\nparent ${lastCommit}\n\n${message}"
                    val shaCommit = sha1Hash(contentCommit)
                    getFileSubDir(repo, shaCommit).createFileIfNotExists(createParents = true).overwrite(contentCommit)
                    currentBranch.overwrite(shaCommit)
                    println(s"[${currentBranch.name} ${shaCommit.slice(0,8)}] ${message}")
                  }
                case Right(error) => println(error)
              }

            }

          }
        } else {
          println(s"On branch ${currentBranch.name}\n\nInitial commit\n\nNothing to commit")
        }

      case Right(error) => println(error)
    }
  }

  /**
   * src -> SHA-1
   */
  def getMapFromCommit(repo: File, sha1Commit: String): Either[Map[String, String], String] = {
    val commitFile = repo/".sgit"/"objects"/sha1Commit.substring(0, 2)/sha1Commit.substring(2)

    if(commitFile.exists) {
      Left(getMapFromCommitIterator(commitFile.lineIterator))
    } else {
      Right(s"commit ${sha1Commit} not found")
    }
  }

  def getMapFromCommitIterator(iterator: Iterator[String]): Map[String, String] ={
    if (iterator.hasNext){
      val line = iterator.next()
      val lineSplit = line.split(" ")
      if(lineSplit.length == 2){
        Map(lineSplit(0) -> lineSplit(1)) ++ getMapFromCommitIterator(iterator)
      } else {
        Map("msg" -> iterator.next())
      }
    } else {
      Map()
    }
  }

  /**
   * Return the Sha-1 of the main tree
   *
   * @param srcs
   * @return
   */
  @tailrec
  def tree(srcs: List[List[String]], size: Int, mapParent: Map[String, List[String]] = Map(), mapIndex: Map[String, String], repo: File): String = {
    if (size == 0) {
      val contentTreeCommit = mapParent("") mkString "\n"
      val sha = sha1Hash(contentTreeCommit)
      getFileSubDir(repo, sha).createFileIfNotExists(createParents = true).overwrite(contentTreeCommit)
      sha
    } else {
      val filesStep =
        srcs
          .filter(arr => arr.length == size)
          .map(arr => arr mkString separator)
          .distinct

      //files
      val blobsStep = filesStep.filter(f => mapIndex.contains(f))
      val blobParents = blobsStep.map(blob => blob.split(separator).slice(0, blob.split(separator).length -1) mkString separator)
      val blobChildren = blobsStep.map(blob => "blob " + mapIndex(blob) + " " + blob.split(separator).last)
      val mapParentPostFiles = updateMapParent(mapParent, blobParents, blobChildren)

      //directory
      val dirsStep = filesStep.filter(f => !mapIndex.contains(f))
      val treeParents = dirsStep.map(dir => dir.split(separator).slice(0, dir.split(separator).length -1) mkString separator)
      val treeChildren = dirsStep.map(dir => "tree " + sha1Hash(mapParentPostFiles(dir) mkString "\n") + " " + dir.split(separator).last)
      val mapParentPostTree = updateMapParent(mapParentPostFiles, treeParents, treeChildren)

      val contentTreeList = dirsStep.map(d => mapParentPostFiles(d) mkString "\n")
      contentTreeList.map(content => getFileSubDir(repo, sha1Hash(content)).createFileIfNotExists(createParents = true).overwrite(content))

      val srcsSliced = srcs.map(arr => removeLastMax(arr, size))
      tree(srcs = srcsSliced, size = size - 1, mapIndex = mapIndex, repo = repo, mapParent = mapParentPostTree)
    }
  }

  def getFileSubDir(repo: File, sha1: String) : File = {
    val dirTree = sha1.substring(0,2)
    val nameTree = sha1.substring(2)
    (repo/".sgit"/"objects"/dirTree/nameTree)
  }

  def removeLastMax(a: List[String], size: Int): List[String] = {
    if (a.length == size) {
      a.slice(0, a.length - 1)
    } else {
      a
    }
  }

  @tailrec
  def updateMapParent(mapParent: Map[String, List[String]],listParents: List[String], listChildren: List[String]): Map[String, List[String]] = {
    if (listParents == Nil){
      mapParent
    } else {
      val mapParentUpdate = updateMapParentElement(mapParent, listParents.head, listChildren.head)
      updateMapParent(mapParentUpdate, listParents.tail, listChildren.tail)
    }

  }

  def updateMapParentElement(mapParent: Map[String, List[String]], parent: String, child: String): Map[String, List[String]] = {
    if (mapParent.contains(parent)){
      val oldList = mapParent(parent)
      val newList = oldList.patch(0, List(child), 0)
      mapParent + (parent -> newList)
    } else {
      mapParent + (parent -> List(child))
    }
  }

}

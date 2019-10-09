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
        val separator = Pattern.quote(System.getProperty("file.separator"))
        val listSorted = keys.toList.map(src => src.split(separator)).sortBy(f => f.length).reverse
        if (listSorted.nonEmpty) {
          val sizeMax = listSorted.head.length
          val treeCommit = tree(srcs = listSorted, size = sizeMax, mapIndex = mapIndex, repo = repo)

          val headFile = (repo/".sgit"/"HEAD").contentAsString.split(" ")(1)
          val currentBranch = (repo/".sgit"/headFile).createFileIfNotExists()
          val lastCommit = currentBranch.contentAsString

          if(lastCommit.isEmpty){
            val contentCommit = s"tree ${treeCommit}\n\n${message}"
            val shaCommit = sha1Hash(contentCommit)
            getFileSubDir(repo, shaCommit).createFileIfNotExists(true).overwrite(contentCommit)
            currentBranch.overwrite(shaCommit)
          } else {
            val contentCommit = s"tree ${treeCommit}\nparent ${lastCommit}\n\n${message}"
            val shaCommit = sha1Hash(contentCommit)
            getFileSubDir(repo, shaCommit).createFileIfNotExists(true).overwrite(contentCommit)
            currentBranch.overwrite(shaCommit)
          }

        }

      case Right(error) => println(error)
    }
  }

  /**
   * Return the Sha-1 of the main tree
   *
   * @param srcs
   * @return
   */
  @tailrec
  def tree(srcs: List[Array[String]], size: Int, mapParent: Map[String, Array[String]] = Map(), mapIndex: Map[String, String], repo: File): String = {
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

  def removeLastMax(a: Array[String], size: Int): Array[String] = {
    if (a.length == size) {
      a.slice(0, a.length - 1)
    } else {
      a
    }
  }

  @tailrec
  def updateMapParent(mapParent: Map[String, Array[String]],listParents: List[String], listChildren: List[String]): Map[String, Array[String]] = {
    if (listParents == Nil){
      mapParent
    } else {
      val mapParentUpdate = updateMapParentElement(mapParent, listParents.head, listChildren.head)
      updateMapParent(mapParentUpdate, listParents.tail, listChildren.tail)
    }

  }

  def updateMapParentElement(mapParent: Map[String, Array[String]], parent: String, child: String): Map[String, Array[String]] = {
    if (mapParent.contains(parent)){
      val oldArray = mapParent(parent)
      val newArray = oldArray.patch(0, Array(child), 0)
      mapParent + (parent -> newArray)
    } else {
      mapParent + (parent -> Array(child))
    }
  }

}

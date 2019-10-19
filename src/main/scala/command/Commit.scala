package command

import util.FileTool._
import better.files._
import objects.Index
import java.util.regex.Pattern
import java.io.File.separator
import annotation.tailrec
import util.BranchTool
import util.CommitTool
import util.ObjectTool

case class Commit(repo: File) {

  def commit(message: String): String = {
    val index = Index(repo)
    index.getMapFromIndex() match {
      case Right(mapIndex) =>
        val keys = mapIndex.keySet
        BranchTool(repo).getCurrentHeadFile match {
          case Right(currentBranch) =>
            if (keys.nonEmpty || CommitTool(repo).isThereACommit) {
              val separator =
                Pattern.quote(System.getProperty("file.separator"))
              val listSorted = keys.toList
                .map(src => src.split(separator).toList)
                .sortBy(f => f.length)
                .reverse
              if (listSorted.nonEmpty) {
                val sizeMax = listSorted.head.length
                val treeCommit = tree(
                  srcs = listSorted,
                  size = sizeMax,
                  mapIndex = mapIndex
                )

                val lastCommit = currentBranch.contentAsString
                val objectTool = ObjectTool(repo)
                if (lastCommit.isEmpty) {
                  val contentCommit = s"tree ${treeCommit}\n\n${message}"
                  val shaCommit = sha1Hash(contentCommit)
                  objectTool
                    .getObjectFromSha(shaCommit)
                    .createFileIfNotExists(createParents = true)
                    .overwrite(contentCommit)
                  currentBranch.overwrite(shaCommit)
                  s"[${currentBranch.name} (root-commit) ${shaCommit
                    .slice(0, 8)}] ${message}"
                } else {
                  CommitTool(repo).getMapFromCommit(lastCommit) match {
                    case Right(mapCommit) =>
                      if (treeCommit == mapCommit("tree")) {

                        s"On branch ${currentBranch.name}\nNothing to commit, working tree clean"

                      } else {
                        val contentCommit =
                          s"tree ${treeCommit}\nparent ${lastCommit}\n\n${message}"
                        val shaCommit = sha1Hash(contentCommit)
                        objectTool
                          .getObjectFromSha(shaCommit)
                          .createFileIfNotExists(createParents = true)
                          .overwrite(contentCommit)
                        currentBranch.overwrite(shaCommit)

                        s"[${currentBranch.name} ${shaCommit.slice(0, 8)}] ${message}"

                      }
                    case Left(error) => error
                  }

                }

              } else {
                ""
              }
            } else {
              s"On branch ${currentBranch.name}\n\nInitial commit\n\nNothing to commit"

            }
          case Left(error) => error
        }
      case Left(error) => error
    }
  }

  /**
    * Return the Sha-1 of the main tree
    *
    * @param srcs
    * @return
    */
  @tailrec
  private def tree(
      srcs: List[List[String]],
      size: Int,
      mapParent: Map[String, List[String]] = Map(),
      mapIndex: Map[String, String]
  ): String = {
    val objectTool = ObjectTool(repo)
    if (size == 0) {
      val contentTreeCommit = mapParent("") mkString "\n"
      val sha = sha1Hash(contentTreeCommit)
      objectTool
        .getObjectFromSha(sha)
        .createFileIfNotExists(createParents = true)
        .overwrite(contentTreeCommit)
      sha
    } else {
      val filesStep =
        srcs
          .filter(arr => arr.length == size)
          .map(arr => arr mkString separator)
          .distinct

      //files
      val blobsStep = filesStep.filter(f => mapIndex.contains(f))
      val blobParents = blobsStep.map(
        blob =>
          blob
            .split(separator)
            .slice(0, blob.split(separator).length - 1) mkString separator
      )
      val blobChildren = blobsStep.map(
        blob => "blob " + mapIndex(blob) + " " + blob.split(separator).last
      )
      val mapParentPostFiles =
        updateMapParent(mapParent, blobParents, blobChildren)

      //directory
      val dirsStep = filesStep.filter(f => !mapIndex.contains(f))
      val treeParents = dirsStep.map(
        dir =>
          dir
            .split(separator)
            .slice(0, dir.split(separator).length - 1) mkString separator
      )
      val treeChildren = dirsStep.map(
        dir =>
          "tree " + sha1Hash(mapParentPostFiles(dir) mkString "\n") + " " + dir
            .split(separator)
            .last
      )
      val mapParentPostTree =
        updateMapParent(mapParentPostFiles, treeParents, treeChildren)

      val contentTreeList =
        dirsStep.map(d => mapParentPostFiles(d) mkString "\n")
      contentTreeList.map(
        content =>
          objectTool
            .getObjectFromSha(sha1Hash(content))
            .createFileIfNotExists(createParents = true)
            .overwrite(content)
      )

      val srcsSliced = srcs.map(arr => removeLastMax(arr, size))
      tree(
        srcs = srcsSliced,
        size = size - 1,
        mapIndex = mapIndex,
        mapParent = mapParentPostTree
      )
    }
  }

  private def removeLastMax(a: List[String], size: Int): List[String] = {
    if (a.length == size) {
      a.slice(0, a.length - 1)
    } else {
      a
    }
  }

  @tailrec
  private def updateMapParent(
      mapParent: Map[String, List[String]],
      listParents: List[String],
      listChildren: List[String]
  ): Map[String, List[String]] = {
    if (listParents == Nil) {
      mapParent
    } else {
      val mapParentUpdate =
        updateMapParentElement(mapParent, listParents.head, listChildren.head)
      updateMapParent(mapParentUpdate, listParents.tail, listChildren.tail)
    }

  }

  private def updateMapParentElement(
      mapParent: Map[String, List[String]],
      parent: String,
      child: String
  ): Map[String, List[String]] = {
    if (mapParent.contains(parent)) {
      val oldList = mapParent(parent)
      val newList = oldList.patch(0, List(child), 0)
      mapParent + (parent -> newList)
    } else {
      mapParent + (parent -> List(child))
    }
  }

}

package util
import better.files._
import java.io.File.separator

import scala.annotation.tailrec

case class CommitTool(repo: File) {

  /**
    * src -> SHA-1
    */
  def getMapFromCommit(
      sha1Commit: String
  ): Either[String, Map[String, String]] = {

    @tailrec
    def loop(
        lines: List[String],
        mapCommit: Map[String, String]
    ): Map[String, String] = {

      @tailrec
      def getMessage(lines: List[String], message: String): String = {
        if (lines.nonEmpty) {
          val line = lines.head
          if (message.nonEmpty) {
            getMessage(lines.tail, s"${message}\n${line}")
          } else {
            getMessage(lines.tail, s"${line}")
          }
        } else {
          message
        }
      }

      if (lines.nonEmpty) {
        val line = lines.head
        val lineSplit = line.split(" ")
        if (lineSplit.length == 2) {
          val newMap = mapCommit + (lineSplit(0) -> lineSplit(1))
          loop(lines.tail, newMap)
        } else {
          val newMap = mapCommit + ("msg" -> getMessage(lines.tail, ""))
          newMap
        }
      } else {
        mapCommit
      }
    }

    ObjectTool(repo).getFileFromSha(sha1Commit) match {
      case Right(commitFile) => Right(loop(commitFile.lines.toList, Map()))
      case Left(error)       => Left(error)
    }
  }

  def getMapBlobCommit(
      shaCommit: String
  ): Either[String, Map[String, String]] = {
    getMapFromCommit(shaCommit) match {
      case Right(mapCommit) =>
        val treeCommit = mapCommit("tree")
        getBlobMapFromTree(treeCommit)
      case Left(error) => Left(error)
    }
  }

  /**
    * src -> SHA-1
    */
  def getBlobMapFromTree(
      sha1Commit: String,
      parent: String = ""
  ): Either[String, Map[String, String]] = {

    def loop(
        linesTree: List[String],
        mapTree: Map[String, String],
        parent: String
    ): Either[String, Map[String, String]] = {
      if (linesTree.nonEmpty) {
        val lineCurrent = linesTree.head
        val lineSplit = lineCurrent.split(" ")
        if (lineSplit.length == 3) {
          val name = lineSplit(2)
          val sha = lineSplit(1)
          val newPath = if (parent == "") name else parent + separator + name

          if (lineSplit(0) == "tree") {

            val newMapTree = mapTree
            ObjectTool(repo).getFileFromSha(sha) match {
              case Right(subTreeFile) =>
                loop(subTreeFile.lines.toList, newMapTree, newPath) match {
                  case Left(error) => Left(error)
                  case Right(mapTreeNext) =>
                    loop(linesTree.tail, mapTreeNext, parent) match {
                      case Left(error)            => Left(error)
                      case Right(mapTreeNextLine) => Right(mapTreeNextLine)
                    }
                }
              case Left(error) => Left(error)
            }

          } else { // blob
            val newMapTree = mapTree + (newPath -> sha)

            loop(linesTree.tail, newMapTree, parent) match {
              case Right(mapTreeNextLine) => Right(mapTreeNextLine)
              case Left(error)            => Left(error)
            }
          }
        } else {
          Left("bad format in tree")
        }
      } else {
        Right(mapTree)
      }
    }

    ObjectTool(repo).getFileFromSha(sha1Commit) match {
      case Right(treeFile) =>
        loop(treeFile.lines.toList, Map(), parent) match {
          case Right(mapTree) => Right(mapTree)
          case Left(error)    => Left(error)
        }
      case Left(error) => Left(error)
    }
  }

  /**
    * List the commit and its ancestors in a map format if the commit exist
    * add the sha of the commit in the map
    */
  def listMapCommit(shaCommit: String): List[Map[String, String]] = {

    @tailrec
    def retrieveParentCommit(
        listCommit: List[Map[String, String]]
    ): List[Map[String, String]] = {
      val currentCommit = listCommit.last
      if (currentCommit.contains("parent")) {
        getMapFromCommit(currentCommit("parent")) match {
          case Left(error) => listCommit
          case Right(mapParentCommit) =>
            val newList = (listCommit :+ (mapParentCommit + ("name" -> currentCommit(
              "parent"
            ))))
            retrieveParentCommit(newList)
        }
      } else {
        listCommit
      }
    }

    getMapFromCommit(shaCommit) match {
      case Left(_) => List()
      case Right(mapCommit) =>
        val listInit = List(mapCommit + ("name" -> shaCommit))
        retrieveParentCommit(listInit)
    }
  }

  def lastCommitSha: Option[String] = {
    BranchTool(repo).getCurrentHeadFile match {
      case Left(error) => None
      case Right(currentBranch) =>
        if (currentBranch.contentAsString.nonEmpty) {
          Some(currentBranch.contentAsString)
        } else {
          None
        }
    }
  }

  def isThereACommit: Boolean = {
    BranchTool(repo).getCurrentHeadFile match {
      case Left(error) => false
      case Right(currentBranch) =>
        if (currentBranch.contentAsString.nonEmpty) {
          true
        } else {
          false
        }
    }
  }
}

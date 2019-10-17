package util
import better.files._
import util.BranchTool.getCurrentBranch
import util.ObjectTool.getFileFromSha
import java.io.File.separator

import scala.annotation.tailrec

object CommitTool {

  /**
    * src -> SHA-1
    */
  def getMapFromCommit(
      repo: File,
      sha1Commit: String
  ): Either[String, Map[String, String]] = {

    @tailrec
    def loop(
        lines: List[String],
        mapCommit: Map[String, String]
    ): Map[String, String] = {
      if (lines.nonEmpty) {
        val line = lines.head
        val lineSplit = line.split(" ")
        if (lineSplit.length == 2) {
          val newMap = mapCommit + (lineSplit(0) -> lineSplit(1))
          loop(lines.tail, newMap)
        } else {
          val lineMsg = lines.tail.head
          val newMap = mapCommit + ("msg" -> lineMsg)
          newMap
        }
      } else {
        mapCommit
      }
    }

    getFileFromSha(repo, sha1Commit) match {
      case Right(commitFile) => Right(loop(commitFile.lines.toList, Map()))
      case Left(error)       => Left(error)
    }
  }

  def getMapBlobCommit(
      repo: File,
      shaCommit: String
  ): Either[String, Map[String, String]] = {
    getCurrentBranch(repo) match {
      case Right(currentBranch) =>
        val lastCommit = currentBranch.contentAsString
        getMapFromCommit(repo, lastCommit) match {
          case Right(mapCommit) =>
            val treeCommit = mapCommit("tree")
            getBlobMapFromTree(repo, treeCommit)
          case Left(error) => Left(error)
        }
      case Left(error) => Left(error)
    }
  }

  /**
    * src -> SHA-1
    */
  def getBlobMapFromTree(
      repo: File,
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
            getFileFromSha(repo, sha) match {
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

    getFileFromSha(repo, sha1Commit) match {
      case Right(treeFile) =>
        loop(treeFile.lines.toList, Map(), parent) match {
          case Right(mapTree) => Right(mapTree)
          case Left(error)    => Left(error)
        }
      case Left(error) => Left(error)
    }
  }

  def isThereACommit(repo: File): Boolean = {
    getCurrentBranch(repo) match {
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

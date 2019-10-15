package util
import better.files._
import util.BranchTool.getCurrentBranch
import util.ObjectUtil.getFileFromSha

import scala.annotation.tailrec

object CommitUtil {
  def getMapBlobCommit(repo: File, shaCommit: String): Either[String, Map[String, String]] = {
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
  def getMapFromCommit(repo: File, sha1Commit: String): Either[String, Map[String, String]] = {

    @tailrec
    def getMapFromCommitIterator(iterator: Iterator[String], mapCommit: Map[String, String]): Map[String, String] ={
      if (iterator.hasNext){
        val line = iterator.next()
        val lineSplit = line.split(" ")
        if(lineSplit.length == 2){
          val newMap = mapCommit + (lineSplit(0) -> lineSplit(1))
          getMapFromCommitIterator(iterator, newMap)
        } else {
          val newMap = mapCommit + (("msg" -> iterator.next()))
          getMapFromCommitIterator(iterator, newMap)
        }
      } else {
        mapCommit
      }
    }

    getFileFromSha(repo, sha1Commit) match {
      case Right(commitFile) => Right(getMapFromCommitIterator(commitFile.lineIterator, Map()))
      case Left(error) => Left(error)
    }
  }



  /**
   * src -> SHA-1
   * need refactoring
   */
  def getBlobMapFromTree(repo: File, sha1Commit: String, parent : String = ""): Either[String, Map[String, String]] = {

    def getMapFromTreeIterator(iterator: Iterator[String], mapTree: Map[String, String], parent: String): Either[String, Map[String, String]] = {
      if (iterator.hasNext){
        val line = iterator.next()
        val lineSplit = line.split(" ")
        if(lineSplit.length == 3){
          if (lineSplit(0) == "tree"){
            val newParent = if(parent == "") lineSplit(2) else parent + "/"+lineSplit(2)
            getBlobMapFromTree(repo, lineSplit(1), newParent) match {
              case Right(mapTree) =>
                getMapFromTreeIterator(iterator, mapTree, parent) match {
                  case Right(mapIterator) =>
                    Right(mapIterator)
                  case Left(error) => Left(error)
                }
              case Left(error) => Left(error)
            }

          } else { // blob
            val blobSrc = if(parent == "") lineSplit(2) else parent+"/"+lineSplit(2)

            val newMap = mapTree + (blobSrc -> lineSplit(1))
            getMapFromTreeIterator(iterator, newMap, parent) match {
              case Right(mapIterator) =>
                Right(mapIterator)
              case Left(error) => Left(error)
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
      case Right(treeFile) => getMapFromTreeIterator(treeFile.lineIterator, Map(), parent) match {
        case Right(mapTree) => Right(mapTree)
        case Left(error) => Left(error)
      }
      case Left(error) => Left(error)
    }
  }


}

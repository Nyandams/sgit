package util
import better.files._
import util.BranchTool.getCurrentBranch
import util.ObjectUtil.getFileFromSha

import scala.annotation.tailrec

object CommitUtil {
  def getMapBlobCommit(repo: File, shaCommit: String): Either[Map[String, String], String] = {
    getCurrentBranch(repo) match {
      case Left(currentBranch) =>
        val lastCommit = currentBranch.contentAsString
        getMapFromCommit(repo, lastCommit) match {
          case Left(mapCommit) =>
            val treeCommit = mapCommit("tree")
            getBlobMapFromTree(repo, treeCommit)
          case Right(error) => Right(error)
        }
      case Right(error) => Right(error)
    }
  }

  /**
   * src -> SHA-1
   */
  def getMapFromCommit(repo: File, sha1Commit: String): Either[Map[String, String], String] = {

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
      case Left(commitFile) => Left(getMapFromCommitIterator(commitFile.lineIterator, Map()))
      case Right(error) => Right(error)
    }
  }



  /**
   * src -> SHA-1
   * need refactoring
   */
  def getBlobMapFromTree(repo: File, sha1Commit: String, parent : String = ""): Either[Map[String, String], String] = {

    def getMapFromTreeIterator(iterator: Iterator[String], mapTree: Map[String, String], parent: String): Either[Map[String, String], String] = {
      if (iterator.hasNext){
        val line = iterator.next()
        val lineSplit = line.split(" ")
        if(lineSplit.length == 3){
          if (lineSplit(0) == "tree"){
            val newParent = if(parent == "") lineSplit(2) else parent + "/"+lineSplit(2)
            getBlobMapFromTree(repo, lineSplit(1), newParent) match {
              case Left(mapTree) =>
                getMapFromTreeIterator(iterator, mapTree, parent) match {
                  case Left(mapIterator) =>
                    Left(mapIterator)
                  case Right(error) => Right(error)
                }
              case Right(error) => Right(error)
            }

          } else { // blob
            val blobSrc = if(parent == "") lineSplit(2) else parent+"/"+lineSplit(2)

            val newMap = mapTree + (blobSrc -> lineSplit(1))
            getMapFromTreeIterator(iterator, newMap, parent) match {
              case Left(mapIterator) =>
                Left(mapIterator)
              case Right(error) => Right(error)
            }
          }
        } else {
          Right("bad format in tree")
        }
      } else {
        Left(mapTree)
      }
    }
    getFileFromSha(repo, sha1Commit) match {
      case Left(treeFile) => getMapFromTreeIterator(treeFile.lineIterator, Map(), parent) match {
        case Left(mapTree) => Left(mapTree)
        case Right(error) => Right(error)
      }
      case Right(error) => Right(error)
    }
  }


}

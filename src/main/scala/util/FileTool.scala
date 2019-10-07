package util

import java.math.BigInteger
import annotation.tailrec
import better.files._
import java.security.MessageDigest

object FileTool {

  def getUserPath: String = {
    System.getProperty("user.dir")
  }

  def getUserDirectory: File = {
    File(getUserPath)
  }

  /**
   * Returns the .sgit path if there is one
   * @param file current file where we check if .sgit exists
   * @return
   */
  @tailrec
  def getSgitRec(file: File = getUserDirectory): Either[File, String] ={
    val sgitDir = file/".sgit"
    if (sgitDir.isDirectory){
      Left(sgitDir)
    } else if(file.parent == null){
      Right("sgit repository not found")
    } else {
      getSgitRec(file.parent)
    }
  }

  /**
   * Return true if the path is in a sgit repository
   */
  @tailrec
  def isInSgit(file: File = getUserDirectory): Boolean = {
    val sgitDir = file/".sgit"
    if (sgitDir.isDirectory){
      true
    } else if(file.parent == null){
      false
    } else {
      isInSgit(file.parent)
    }
  }

  /**
   * Apply SHA-1 to a string
   * @param content String
   * @return String
   */
  def sha1(content: String): String = {
    String.format("%032x", new BigInteger(1, MessageDigest.getInstance("SHA-1").digest(content.getBytes("UTF-8"))))  }
}
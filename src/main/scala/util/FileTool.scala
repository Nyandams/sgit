package util

import java.math.BigInteger
import annotation.tailrec
import better.files._
import java.security.MessageDigest

object FileTool {

  def getUserPath: String = System.getProperty("user.dir")

  def getUserDirectory: File = File(getUserPath)

  /**
   * Returns the .sgit path if there is one
   *
   * @param file current file where we check if .sgit exists
   * @return
   */
  @tailrec
  def getSgitRec(file: File = getUserDirectory): Either[String, File] = {
    val sgitDir = file / ".sgit"
    if (sgitDir.isDirectory) {
      Right(file)
    } else if (file.parent == null) {
      Left("sgit repository not found")
    } else {
      getSgitRec(file.parent)
    }
  }

  /**
   * Return true if the path is in a sgit repository
   */
  @tailrec
  def isInSgit(file: File = getUserDirectory): Boolean = {
    val sgitDir = file / ".sgit"
    if (sgitDir.isDirectory) {
      true
    } else if (file.parent == null) {
      false
    } else {
      isInSgit(file.parent)
    }
  }

  // Bug Sha1 with folder d3 containing d3f2 and d3f1 file, only 39chars
  def sha1Hash(s: String): String = {
    val md = MessageDigest.getInstance("SHA1")
    val digest = md.digest(s.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hashedString = bigInt.toString(16)
    hashedString
  }

  def allFileRepoSet(repo: File): Set[String] ={
    repo.listRecursively
      .toSet.filter(f => f.isRegularFile)
      .filter(f => !f.pathAsString.contains(".sgit"))
      .map(f => repo.relativize(f).toString)
  }
}
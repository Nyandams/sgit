package command
import Console.{GREEN, RESET}
import better.files.File
import util.BranchTool._
import util.CommitTool._
import annotation.tailrec

object Branch {
  def newBranch(repo: File, nameBranch: String ): Unit ={
    getCurrentBranch(repo) match {
      case Left(error) => println("Failed to resolve 'HEAD' as a valid ref")
      case Right(currentBranch) =>
        val splitTagName = nameBranch.split(" ")
        if (splitTagName.length == 1){
          val headFolder = (repo/".sgit"/"refs"/"heads")
          if (headFolder.exists){
            if (headFolder.list.contains((repo/".sgit"/"refs"/"heads"/nameBranch))) {
              println(s"branch '${nameBranch}' already exists")
            } else {
              val branchFile = (repo/".sgit"/"refs"/"heads"/nameBranch).createFileIfNotExists(true)
              branchFile.appendText(currentBranch.contentAsString)
            }
          } else {
            println("no heads directory")
          }
        } else {
          println(s"'${nameBranch}' is not a valid tag name")
        }
    }
  }

  def showBranch(repo: File): Unit = {
    getCurrentBranch(repo) match {
      case Left(error) => println(error)
      case Right(fileHead) =>
        val currentBranch = fileHead.name
        val headsFolder = (repo/".sgit"/"refs"/"heads")
        if(headsFolder.exists){
          println(
            headsFolder
              .list
              .map(f => f.name)
              .toList
              .sorted
              .map(name => if(name == currentBranch) s"* ${GREEN}${name}${RESET}" else s"  ${name}") mkString "\n"
          )
        }
    }
  }

  def showBranchVerbose(repo: File): Unit = {
    getCurrentBranch(repo) match {
      case Left(error) => println(error)
      case Right(fileHead) =>
        val currentBranch = fileHead.name
        val headsFolder = (repo/".sgit"/"refs"/"heads")
        if(headsFolder.exists){
          val branchNames = headsFolder
                                  .list
                                  .map(f => f.name)
                                  .toList
                                  .sorted

          val longestBranchName = branchNames.reduceLeft(maxLengthString).length

          println(branchNames.map(name => verbosebranch(repo, (headsFolder/name), currentBranch, longestBranchName)) mkString "\n"
          )
        }
    }
  }

  def maxLengthString(s1: String, s2: String): String = if (s1.length > s2.length) s1 else s2

  def repeatChar(c: Char, n: Int): String = {
    @tailrec
    def repeatCharAcc(c: Char, n: Int, accum: String): String = {
      if (n == 0) accum
      else repeatCharAcc(c, n-1, accum + c)
    }
    repeatCharAcc(c, n, "")
  }

  def verbosebranch(repo: File, branch: File, currentBranch: String, longestBranchName: Int): String = {
    val name = branch.name
    val sha1Commit = branch.contentAsString
    getMapFromCommit(repo, sha1Commit) match {
      case Left(error) => error
      case Right(mapCommit) =>
        val spaceString = repeatChar(' ', longestBranchName - name.length)
        if(name == currentBranch) s"* ${GREEN}${name}${RESET}${spaceString} ${sha1Commit.slice(0,7)} ${mapCommit("msg")}"
        else s"  ${name}${spaceString} ${sha1Commit.slice(0,7)} ${mapCommit("msg")}"
    }
  }
}
package command
import Console.{GREEN, RESET}
import better.files.File
import util.BranchTool
import util.CommitTool

import annotation.tailrec

case class Branch(repo: File) {
  def newBranch(nameBranch: String): String = {
    val commitTool = CommitTool(repo)
    if (commitTool.isThereACommit) {
      val branchTool = BranchTool(repo)
      val newBranch = branchTool.getBranchFile(nameBranch)
      if (newBranch.exists){
        s"branch '${nameBranch}' already exists"
      } else {
        newBranch.createFileIfNotExists(true)
        newBranch.appendText(commitTool.lastCommitSha.get)
        ""
      }
    } else {
      s"Not a valid object name: 'master'."
    }

  }

  def showBranch: String = {
    if (CommitTool(repo).isThereACommit) {
      val branchTool = BranchTool(repo)
      branchTool.getCurrentHeadFile match {
        case Left(error) => error
        case Right(fileHead) =>
          val currentBranch = fileHead.name
          branchTool
            .getListBranchNames
            .map(name => if (name == currentBranch) s"* ${GREEN}${name}${RESET}" else s"  ${name}") mkString "\n"
      }
    } else {
      ""
    }
  }

  def showBranchVerbose: String = {
    if (CommitTool(repo).isThereACommit) {
      val branchTool = BranchTool(repo)
      branchTool.getCurrentHeadFile match {
        case Left(error) => error
        case Right(fileHead) =>
          val currentBranch = fileHead.name
          val headsFolder = branchTool.getHeadsFolder
          val branchNames = branchTool.getListBranchNames

          val longestBranchName = branchNames.reduceLeft(maxLengthString).length

          (branchNames.map(
            name =>
              verbosebranch(
                (headsFolder / name),
                currentBranch,
                longestBranchName
              )
          ) mkString "\n")
      }
    } else {
      ""
    }

  }

  private def maxLengthString(s1: String, s2: String): String =
    if (s1.length > s2.length) s1 else s2

  private def repeatChar(c: Char, n: Int): String = {
    @tailrec
    def repeatCharAcc(c: Char, n: Int, accum: String): String = {
      if (n == 0) accum
      else repeatCharAcc(c, n - 1, accum + c)
    }
    repeatCharAcc(c, n, "")
  }

  private def verbosebranch(
      branch: File,
      currentBranch: String,
      longestBranchName: Int
  ): String = {
    val name = branch.name
    val sha1Commit = branch.contentAsString
    CommitTool(repo).getMapFromCommit(sha1Commit) match {
      case Left(error) => error
      case Right(mapCommit) =>
        val spaceString = repeatChar(' ', longestBranchName - name.length)
        if (name == currentBranch)
          s"* ${GREEN}${name}${RESET}${spaceString} ${sha1Commit.slice(0, 7)} ${mapCommit("msg")}"
        else
          s"  ${name}${spaceString} ${sha1Commit.slice(0, 7)} ${mapCommit("msg")}"
    }
  }
}

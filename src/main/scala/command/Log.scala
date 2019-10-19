package command

import better.files._

import Console.{RESET, YELLOW}
import util.ObjectTool
import util.CommitTool
import java.nio.file._
import java.util.Date

import util.BranchTool

import scala.annotation.tailrec

case class Log(repo: File) {
  def log: String = {

    @tailrec
    def loop(listCommits: List[Map[String, String]], log: String): String = {
      if(listCommits.nonEmpty){
        val currentCommit = listCommits.head
        ObjectTool(repo).getFileFromSha(currentCommit("name")) match {
          case Left(error) => error
          case Right(commitFile) =>
            val date = new Date(Files.getLastModifiedTime(commitFile.path).toMillis)
            val commitMsg = currentCommit("msg").split("\n") mkString "\n    "
            val newLog = log + s"${YELLOW}commit ${currentCommit("name")}${RESET}\nDate:   ${date}\n\n    ${commitMsg}\n\n"
            loop(listCommits.tail, newLog)
        }
      } else {
        log
      }
    }

    BranchTool(repo).getCurrentHeadFile match {
      case Left(error) => error + "\n"
      case Right(branch) =>
        val commitTool = CommitTool(repo)
        val listCommits = commitTool.listMapCommit(commitTool.lastCommitSha.get)
        if (listCommits.nonEmpty) {
          loop(listCommits, "")
        } else {
          s"your current branch '${branch.name}' does not have any commits yet\n"
        }
    }
  }
}

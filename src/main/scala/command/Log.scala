package command

import better.files._

import Console.{RESET, YELLOW}
import util.ObjectTool.getFileFromSha
import util.BranchTool.getCurrentBranch
import util.CommitTool.getMapFromCommit
import java.nio.file._
import java.util.Date

import scala.annotation.tailrec


object Log {
 def log(repo: File) = {

   @tailrec
   def loop(repo: File, shaCommit: String, log: String): String = {
     getMapFromCommit(repo, shaCommit) match {
       case Left(error) => error
       case Right(mapCommit) =>

           getFileFromSha(repo, shaCommit) match {
             case Left(error) => error
             case Right(commitFile) =>
               val date = new Date(Files.getLastModifiedTime(commitFile.path).toMillis)
               val newLog = log + s"${YELLOW}commit ${shaCommit}${RESET}\nDate:   ${date}\n\n    ${mapCommit("msg")}\n\n"
               if (mapCommit.contains("parent")) {
                 loop(repo, mapCommit("tree"), newLog)
               } else {
                 newLog
               }
           }
     }
   }

   getCurrentBranch(repo) match {
     case Left(error) => println(error)
     case Right(branch) =>
       val lastCommit = branch.contentAsString
       if (lastCommit.nonEmpty){
         println(loop(repo, lastCommit, ""))
       } else {
         println(s"your current branch '${branch.name}' does not have any commits yet")
       }
   }
 }
}

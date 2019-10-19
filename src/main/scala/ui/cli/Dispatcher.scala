package ui.cli
import command.Add
import command.Commit._
import command.Init
import command.Status._
import command.Rm
import command.Tag
import command.Diff._
import command.Log
import command.Branch
import command.Checkout
import ui.cli.Parser.getConfig
import util.FileTool._

object Dispatcher {
  def dispatch(arguments: Array[String]): Unit = {
    getSgitRec() match {
      // command that need a repo to be executed
      case Right(repo) =>
        getConfig(arguments) match {
          case Some(config) =>
            config.mode match {
              case "init"   => println(Init().init)
              case "add"    => print(Add(repo).add(config.files))
              case "rm"     => print(Rm(repo).rm(config.files))
              case "commit" => println(commit(repo, config.commitMessage))
              case "status" => print(status(repo))
              case "tag" =>
                if (config.tagName.nonEmpty)
                  println(Tag(repo).newTag(config.tagName))
                else println(Tag(repo).showTags)
              case "diff" => print(diff(repo))
              case "log"  => print(Log(repo).log)
              case "branch" =>
                if (config.file.nonEmpty) println(Branch(repo).newBranch(config.file))
                else if (config.verbose) println(Branch(repo)showBranchVerbose)
                else println(Branch(repo).showBranch)
              case "checkout" => println(Checkout(repo).checkout(config.file))
            }
          case None =>
          case _    =>
        }
      //  command that doesn't need a repo to be launched
      case Left(error) =>
        getConfig(arguments) match {
          case Some(config) =>
            config.mode match {
              case "init" => println(Init().init)
              case _      => println(error)
            }
          case None =>
          case _    =>
        }
    }
  }
}

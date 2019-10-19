package ui.cli
import command.Add._
import command.Commit._
import command.Init._
import command.Status._
import command.Rm._
import command.Tag._
import command.Diff._
import command.Log
import command.Branch._
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
              case "init"   => println(init())
              case "add"    => print(add(repo, config.files))
              case "rm"     => print(rm(repo, config.files))
              case "commit" => println(commit(repo, config.commitMessage))
              case "status" => print(status(repo))
              case "tag" =>
                if (config.tagName.nonEmpty)
                  println(newTag(repo, config.tagName))
                else println(showTags(repo))
              case "diff" => print(diff(repo))
              case "log"  => print(Log(repo).log)
              case "branch" =>
                if (config.file.nonEmpty) println(newBranch(repo, config.file))
                else if (config.verbose) println(showBranchVerbose(repo))
                else println(showBranch(repo))
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
              case "init" => println(init())
              case _      => println(error)
            }
          case None =>
          case _    =>
        }
    }
  }
}

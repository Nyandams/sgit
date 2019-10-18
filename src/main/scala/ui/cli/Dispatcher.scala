package ui.cli
import command.Add._
import command.Commit._
import command.Init._
import command.Status._
import command.Rm._
import command.Tag._
import command.Diff._
import command.Log._
import command.Branch._
import command.Checkout._
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
              case "init"   => init()
              case "add"    => add(repo, config.files)
              case "rm"     => rm(repo, config.files)
              case "commit" => commit(repo, config.commitMessage)
              case "status" => print(status(repo))
              case "tag" =>
                if (config.tagName.nonEmpty) print(newTag(repo, config.tagName))
                else print(showTags(repo))
              case "diff" => diff(repo)
              case "log"  => log(repo)
              case "branch" =>
                if (config.file.nonEmpty) newBranch(repo, config.file)
                else if (config.verbose) showBranchVerbose(repo)
                else showBranch(repo)
              case "checkout" => checkout(repo, config.file)
            }
          case None =>
          case _    =>
        }
      //  command that doesn't need a repo to be launched
      case Left(error) =>
        getConfig(arguments) match {
          case Some(config) =>
            config.mode match {
              case "init" => init()
              case _      => println(error)
            }
          case None =>
          case _    =>
        }
    }
  }
}

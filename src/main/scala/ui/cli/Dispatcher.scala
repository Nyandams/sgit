package ui.cli
import ui.cli.Parser.getConfig
import command.Init._
import command.Add._
import command.Commit._
import util.FileTool._

object Dispatcher {
  def dispatch(arguments: Array[String]): Unit ={
    getSgitRec() match {
      // command that need a repo to be executed
      case Left(repo) => getConfig(arguments) match {
        case Some(config) => config.mode match {
          case "init" => init()
          case "add" => add(repo, config.files)
          case "commit" => commit(repo, config.commitMessage)
        }
        case None =>
        case _ =>
      }
      //  command that doesn't need a repo to be launched
      case Right(error) => getConfig(arguments) match {
        case Some(config) => config.mode match {
          case "init" => init()
          case _ => println(error)
        }
        case None =>
        case _ =>
      }
    }
  }
}

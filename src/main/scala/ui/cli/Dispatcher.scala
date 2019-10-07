package ui.cli
import ui.cli.Parser.getConfig
import command.Init._
import command.Add._
import util.FileTool._

object Dispatcher {
  def dispatch(arguments: Array[String]): Unit ={
    getConfig(arguments) match {
      case Some(config) => config.mode match {
        case "init" => init()
        case "add" => {
          add(config.files)
        }
        case "" =>
        case _ =>

      }
      case None =>
      case _ =>
    }
  }
}

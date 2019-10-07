package ui.cli
import ui.cli.Parser.getConfig
import command.Init._
import util.FileTool._

object Dispatcher {
  def dispatch(arguments: Array[String]): Unit ={
    getConfig(arguments) match {
      case Some(config) => config.mode match {
        case "init" => init()
        case "add" => {
          config.files.foreach(file => println(file))
        }
        case "" =>
        case _ =>

      }
      case None =>
      case _ =>
    }
  }
}

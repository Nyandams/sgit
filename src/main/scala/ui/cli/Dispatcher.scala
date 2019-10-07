package ui.cli
import ui.cli.Parser.getConfig
import command.Init._

object Dispatcher {
  def dispatch(arguments: Array[String]): Unit ={
    getConfig(arguments) match {
      case Some(config) => config.mode match {
        case "init" => init()

      }
      case None =>
    }
  }
}

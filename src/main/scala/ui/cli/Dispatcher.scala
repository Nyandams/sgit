package ui.cli
import ui.cli.Parser.getConfig

object Dispatcher {
  def dispatch(arguments: Array[String]): Unit ={
    getConfig(arguments) match {
      case Some(config) =>
      case None =>
    }
  }
}

package ui.cli

case class Config(
                   mode: String = "",
                   commitMessage: String = "",
                   files: Array[String] = Array(),
                   patch: Boolean = false,
                   stat: Boolean = false,
                   tagName: String = "",
                   verbose: Boolean = false,
                   showBranch: Boolean = false,
                   showTag: Boolean = false,
                   file: String = "",
                   interactive: Boolean = false
                 )

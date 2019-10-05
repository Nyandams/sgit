import java.io.File
case class Config(
                   mode: String = "",
                   commitMessage: String = "",
                   files: Seq[File] = Seq(),
                   regex: String = "",
                   patch: Boolean = false,
                   stat: Boolean = false,
                   tagName: String = "",
                   verbose: Boolean = false,
                   showBranch: Boolean = false,
                   showTag: Boolean = false,
                   file: File = new File("."),
                   interactive: Boolean = false
                 )
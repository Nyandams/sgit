import java.io.File
case class Config(
                   mode: String = "",
                   commitMessage: String = "",
                   files: Seq[File] = Seq(),
                   patch: Boolean = false,
                   stat: Boolean = false,
                   tagName: String = ""
                 )
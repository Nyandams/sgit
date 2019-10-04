import java.io.File

object Application  extends App {
  val parser = new scopt.OptionParser[Config]("sgit") {
    head("sgit", "1.0")

    cmd(name = "init")
      .action((_, c) => c.copy(mode = "init"))
      .text("Create a Sgit repository in the current directory.")

    cmd(name = "status")
      .action((_, c) => c.copy(mode = "status"))
      .text("Show the working tree status.")

    // TODO : . and regexp
    cmd(name = "add")
      .action((_, c) => c.copy(mode = "add"))
      .text("Add file contents to the index.")
      .children(
        arg[File]("<file>...")
          .unbounded()
          .optional()
          .action((x, c) => c.copy(files = c.files :+ x))
          .text("list of files to add")
      )

    cmd(name = "commit")
      .action((_, c) => c.copy(mode = "add"))
      .text("Record changes to the repository")
      .children(
        opt[String](name = "m")
          .required()
          .action((x, c) => c.copy(commitMessage = x))
          .text("commit message")
      )

    cmd(name = "diff")
      .action((_, c) => c.copy(mode = "diff"))
      .text("Show changes between commits, commit and working tree, etc")

    cmd(name = "log")
      .action((_, c) => c.copy(mode = "log"))
      .text("Show all commits started with newest")
      .children(
        opt[Unit]('p', "patch")
          .action((x, c) => c.copy(patch = true))
          .text("Show changes overtime"),

        opt[Unit]('s', "stat")
          .action((x, c) => c.copy(patch = true))
          .text("Show stats about changes overtime"),
      )

    cmd(name = "tag")
      .action((_, c) => c.copy(mode = "tag"))
      .text("Add a tag reference in refs/tags/.")
      .children(
        arg[String]("<tag name>")
          .required()
          .action((x, c) => c.copy(tagName = x))
          .text("name of the tag")
      )
  }

/*
sgit branch <branch name>
Create a new branch
sgit branch -av
List all existing branches and tags
sgit checkout <branch or tag or commit hash>


Merge & Rebase
sgit merge <branch>
sgit rebase <branch>
sgit rebase -i <commit hash or banch name>
*/

  // parser.parse returns Option[C]
  parser.parse(args, Config()) match {
    case Some(config) =>
    // do stuff

    case None =>
    // arguments are bad, error message will have been displayed
  }
}

// TODO : sbt assembly

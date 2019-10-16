package command
import better.files._
import annotation.tailrec
import objects.Index.getMapFromIndex

object Diff {
  def diff(repo: File): Unit = {
    getMapFromIndex(repo) match {
      case Left(error) => println(error)
      case Right(mapIndex) =>
        //val coupleRepoStaged = mapIndex.toList(c => (repo/))
    }
  }



  def constructLCSMatrix(newFile: List[String], oldFile: List[String]): Map[(Int, Int), Int] = {

    @tailrec
    def loop(mapMatrix: Map[(Int, Int), Int], newLine: Int, oldLine: Int): Map[(Int, Int), Int] = {

      // matrix completed
      if (newLine > newFile.length) {
        mapMatrix
      }

      // next line
      else if (oldLine == oldFile.length + 1) {
        loop(mapMatrix, newLine + 1, 0)
      }

      // first line and column with 0 (start of LCS algorithm)
      else if (newLine == 0 || oldLine == 0) {
        val newMapMatrix = mapMatrix + ((newLine, oldLine) -> 0)
        loop(newMapMatrix, newLine, oldLine + 1)
      }

      else {
        val contentNewLine = newFile(newLine - 1)
        val contentOldLine = oldFile(oldLine - 1)

        if (contentNewLine == contentOldLine) {
          val value = mapMatrix((newLine - 1, oldLine - 1)) + 1
          val newMapMatrix = mapMatrix + ((newLine, oldLine) -> value)
          loop(newMapMatrix, newLine, oldLine + 1)
        } else {
          val value = Integer.max(mapMatrix(newLine, oldLine - 1), mapMatrix(newLine - 1, oldLine))
          val newMapMatrix = mapMatrix + ((newLine, oldLine) -> value)
          loop(newMapMatrix, newLine, oldLine + 1)
        }
      }
    }

    loop(Map(), 0, 0)
  }
}

package command
import better.files._
import annotation.tailrec
import objects.Index.getMapFromIndex
import util.ObjectTool.getFileFromShaIncomplete
import Console.{GREEN, RESET, RED}

object Diff {
  def diff(repo: File): String = {
    getMapFromIndex(repo) match {
      case Left(error) => error
      case Right(mapIndex) =>
        val tripletNewOldName = mapIndex.toList.map(
          tuple =>
            (
              (repo / tuple._1),
              getFileFromShaIncomplete(repo, tuple._2).getOrElse(File("error")),
              tuple._1
            )
        )
        getDiffAllFiles(repo, tripletNewOldName)
    }
  }

  def getDiffList(
      mapMatrix: Map[(Int, Int), Int],
      sizeNewFile: Int,
      sizeOldFile: Int
  ): List[(String, Int)] = {

    @tailrec
    def loop(
        newLine: Int,
        oldLine: Int,
        listRep: List[(String, Int)]
    ): List[(String, Int)] = {

      //if all the matrix is read, return the listRep
      if (newLine == 0 && oldLine == 0) {
        listRep
      }

      //if  the cursor is at the top of the matrix, go left
      else if (newLine == 0) {
        val newList = ("-", oldLine) :: listRep
        loop(newLine, oldLine - 1, newList)
      }

      // if the cursor is at the left of the matrix, go up
      else if (oldLine == 0) {
        val newList = ("+", newLine) :: listRep
        loop(newLine - 1, oldLine, newList)
      }

      //if the element at the left is equal to the element at the top
      else if (mapMatrix((newLine, oldLine - 1)) == mapMatrix(
                 newLine - 1,
                 oldLine
               )) {

        // if the element at the top left is equal to us - 1, go to this element
        if (mapMatrix(newLine, oldLine) - 1 == mapMatrix(
              newLine - 1,
              oldLine - 1
            )) {
          loop(newLine - 1, oldLine - 1, listRep)
        }
        //else go to the left
        else {
          val newList = ("-", oldLine) :: listRep
          loop(newLine, oldLine - 1, newList)
        }
      }
      //if the element at the left is not equal the element at the top
      else {

        //if the element at the top is greater than the element at the left, go up
        if (mapMatrix(newLine - 1, oldLine) > mapMatrix(newLine, oldLine - 1)) {
          val newList = ("+", newLine) :: listRep
          loop(newLine - 1, oldLine, newList)
        }
        //if the element at the left is greater than the element at the top, go left
        else {
          val newList = ("-", oldLine) :: listRep
          loop(newLine, oldLine - 1, newList)
        }
      }
    }

    loop(sizeNewFile, sizeOldFile, List())
  }

  def getDiffStringOneFile(
      listDiff: List[(String, Int)],
      newFileLines: List[String],
      oldFileLines: List[String]
  ): String = {

    @tailrec
    def loop(result: String, listDiffUpdated: List[(String, Int)]): String = {

      listDiffUpdated match {
        case Nil => result

        case head :: tail =>
          if (head._1 == "-") {
            val newResult = result + s"${RED}${head._2} - ${oldFileLines(head._2 - 1)} \n${RESET}"
            loop(newResult, tail)
          } else {
            val newResult = result + s"${GREEN}${head._2} + ${newFileLines(head._2 - 1)} \n${RESET}"
            loop(newResult, tail)
          }
      }
    }
    loop("", listDiff.sortWith(_._2 < _._2))
  }

  def getDiffAllFiles(
      repo: File,
      tripletNewOldName: List[(File, File, String)]
  ): String = {

    @tailrec
    def loop(
        tripletNewOldName: List[(File, File, String)],
        result: String
    ): String = {

      tripletNewOldName match {
        case Nil => result
        case head :: tail =>
          val newFileLines = head._1.lines.toList
          val oldFileLines = head._2.lines.toList

          val matrix = constructLCSMatrix(newFileLines, oldFileLines)
          val listDiff =
            getDiffList(matrix, newFileLines.length, oldFileLines.length)

          if (listDiff.nonEmpty) {
            val newResult = result + head._3 + " :\n" + getDiffStringOneFile(
              listDiff,
              newFileLines,
              oldFileLines
            ) + "\n\n"
            loop(tail, newResult)
          } else {
            loop(tail, result)
          }
      }
    }
    loop(tripletNewOldName, "")
  }

  def constructLCSMatrix(
      newFile: List[String],
      oldFile: List[String]
  ): Map[(Int, Int), Int] = {

    @tailrec
    def loop(
        mapMatrix: Map[(Int, Int), Int],
        newLine: Int,
        oldLine: Int
    ): Map[(Int, Int), Int] = {

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
      } else {
        val contentNewLine = newFile(newLine - 1)
        val contentOldLine = oldFile(oldLine - 1)

        if (contentNewLine == contentOldLine) {
          val value = mapMatrix((newLine - 1, oldLine - 1)) + 1
          val newMapMatrix = mapMatrix + ((newLine, oldLine) -> value)
          loop(newMapMatrix, newLine, oldLine + 1)
        } else {
          val value = Integer.max(
            mapMatrix(newLine, oldLine - 1),
            mapMatrix(newLine - 1, oldLine)
          )
          val newMapMatrix = mapMatrix + ((newLine, oldLine) -> value)
          loop(newMapMatrix, newLine, oldLine + 1)
        }
      }
    }
    loop(Map(), 0, 0)
  }
}

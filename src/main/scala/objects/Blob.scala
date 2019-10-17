package objects

import better.files._
import util.FileTool._
import annotation.tailrec
import java.io.File.separator

object Blob {

  /**
    * Create Blobs and add them to the index
    * @param files
    */
  def handleBlobsAdding(
      repo: File,
      files: Array[File],
      index: Int = 0
  ): Map[String, String] = {
    if (index < files.length) {
      handleBlobCreation(repo, files(index)) ++ handleBlobsAdding(
        repo,
        files,
        index + 1
      )
    } else {
      Map()
    }
  }

  /**
    * Create the blob of file and return the map corresponding to the index
    * @param file file that will be used to create a blob
    * @return a Map(src -> SHA-1) containing only the filepath and its blob
    */
  def handleBlobCreation(repo: File, file: File): Map[String, String] = {
    val textFile = file.contentAsString
    val sha = file.sha1.toLowerCase()

    val indexPathCut = repo.relativize(file)

    val dirBlob = sha.substring(0, 2)
    val nameBlob = sha.substring(2)
    (repo / ".sgit" / "objects" / dirBlob / nameBlob)
      .createFileIfNotExists(createParents = true)
      .overwrite(textFile)
    Map(indexPathCut.toString -> sha)
  }
}

package edu.umass.ciir.kbbridge.tac

import java.io.{PrintWriter, File}

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 9/9/13
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */
object RepackageTacDocs extends App {

  packageDir(args(0))

  val pw = new PrintWriter(new File("finished"), "UTF-8")
  pw.println("Done.")
  pw.close()

  def packageDir(dir:String) = {
    val files = fromDirectory(dir)
    println("Num files: " + files.size)
    val outputFile = new File("./data/tac-corpus")
    val n = 10000
    var curBatch = 0
    var p = new PrintWriter(outputFile.getAbsolutePath() + curBatch.toString + ".tactext", "UTF-8")
    for ((file, idx) <- files.zipWithIndex) {
      if (idx % n == 0 && idx > 0) {
        println("Starting batch: " + curBatch + " file: " + file.getAbsolutePath)
        p.close
        curBatch += 1
        p = new PrintWriter(outputFile.getAbsolutePath() + curBatch.toString + ".tactext", "UTF-8")
      }

      val lines = slurpLines(file.getAbsolutePath)
      lines.map(l =>  p.println(l))

    }
    p.close()
  }

  def slurpLines(path: String) = {
    val src = scala.io.Source.fromFile(path, "UTF-8")
    val lines = src.getLines().toIndexedSeq
    src.close()
    lines
  }

  def fromDirectory(dir : String)  = {
    val files = recursiveListFiles(new File(dir)).filter(d => d.getAbsolutePath.endsWith("sgm") || d.getAbsolutePath.endsWith("giga") )
    files
  }

  def recursiveListFiles(f: File): Seq[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

}

package edu.umass.ciir.kbbridge.trec

import org.lemurproject.galago.core.retrieval.ScoredDocument

/**
 * User: jdalton
 * Date: 6/5/13
 */
object TrecRunWriter {



  def writeRunFile(f: java.io.File, results: Map[String, Seq[ScoredDocument]], runId : String = "Default")  {
    val p = new java.io.PrintWriter(f, "UTF-8")
    try {
      for( (query, results) <- results) {
        results.map(r => {
          if (r == null){
            println("where does this null result come from?")
          }
          if (r.documentName == null){
            println("documentname == null "+r)
          }
          if (r.rank == null){
            println("rank == null "+r)
          }
          p.println("%s Q0 %s %d %s %s".format(query, r.documentName, r.rank, "%10.8f".format(r.score), runId))
        })
      }
    } finally { p.close() }
  }


  def writeRunFileFromTuple(f: java.io.File, results: Seq[(String, Seq[(String,Double, Int)])], runId : String = "Default") {
    val p = new java.io.PrintWriter(f, "UTF-8")
    try {
      for( (query, results) <- results) {
        results.map(r => p.println("%s Q0 %s %d %s %s".format(query, r._1, r._3, "%10.8f".format(r._2), runId)))
      }
    } finally { p.close() }
  }


  def writeLines(f: java.io.File, lines: Seq[String]) {
    val p = new java.io.PrintWriter(f, "UTF-8")
    try {
      for( line <- lines) {
        p.println(line)
      }
    } finally { p.close() }
  }

}

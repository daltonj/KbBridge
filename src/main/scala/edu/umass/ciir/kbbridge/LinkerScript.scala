package edu.umass.ciir.kbbridge

import java.io.{PrintWriter, File}
import edu.umass.ciir.kbbridge.tac.TacQueryUtil
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 7/22/13
 * Time: 1:27 PM
 * To change this template use File | Settings | File Templates.
 */
object LinkerScript extends App {

  val allqueries = TacQueryUtil.allQueries
  val docSet = allqueries.map(q => q.docId).toSet
  println("queries: " + allqueries.size + " docs: " + docSet.size)
  writeAnnotationScript(docSet)

  def writeAnnotationScript(docsRequiringAnnotation: Iterable[String]) = {
    val outputFile = new File("./scripts/annotate-tac-swarm-factkb1")
    val n = 100000
    var curBatch = 0
    var p = new PrintWriter(outputFile.getAbsolutePath() + curBatch.toString + ".sh", "UTF-8")
    for ((docSet, idx) <- (docsRequiringAnnotation grouped 5).zipWithIndex) {
      val sb = new StringBuilder
      if (idx % n == 0 && idx > 0) {
        p.close
        curBatch += 1
        p = new PrintWriter(outputFile.getAbsolutePath() + curBatch.toString + ".sh", "UTF-8")
      }

      sb append "qsub -b y " + "-l mem_free=6G -l mem_token=6G" + " -cwd -o ./out/"
      sb append docSet.head
      sb append " -e ./err/"
      sb append docSet.head

      sb append " ./scripts/runAnnotation.sh "
      //  sb append " /work1/allan/jdalton/tacco/scripts/runEntityLinker.sh "

      // input query
      sb append docSet.mkString(",")
      sb append " /work1/allan/jdalton/entity-linking/tac-source-g34"
      sb append " ./tac-nlp-annotations-factkb1"

      // println(sb.toString)
      p.println(sb.toString)
    }
    p.close()
  }

}

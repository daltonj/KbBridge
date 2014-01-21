package edu.umass.ciir.kbbridge

import java.io.{PrintWriter, File}
import edu.umass.ciir.kbbridge.tac.TacQueryUtil

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 7/22/13
 * Time: 1:27 PM
 */
object TacContextMentionLinkerScript extends App {

  val allqueries = TacQueryUtil.selectNonNilEvenOddSplitQueries
  val querySet = allqueries._1 ++ allqueries._2

   val queryIds = querySet.map(_.mentionId)
//  val queries2013 = allqueries("2013")._2
//  val querySet = queries2013.map(q => q.mentionId).toSet
  println("queries: " +  queryIds.size)
  writeAnnotationScript(queryIds)

  def writeAnnotationScript(docsRequiringAnnotation: Iterable[String]) = {
    val outputFile = new File("./scripts/extract-context-features-tac")
    val n = 100000
    var curBatch = 0
    var p = new PrintWriter(outputFile.getAbsolutePath() + curBatch.toString + ".sh", "UTF-8")
    for ((docSet, idx) <- (docsRequiringAnnotation grouped 1).zipWithIndex) {
      val sb = new StringBuilder
      if (idx % n == 0 && idx > 0) {
        p.close
        curBatch += 1
        p = new PrintWriter(outputFile.getAbsolutePath() + curBatch.toString + ".sh", "UTF-8")
      }

      sb append "qsub -b y " + "-l mem_free=8G -l mem_token=8G" + " -cwd -o ./out/"
      sb append docSet.head
      sb append " -e ./err/"
      sb append docSet.head

      sb append " java -server -Xmx10G -Dfile.encoding=utf-8 -cp /work1/allan/jdalton/kbbridge/target/kbbridge-0.1-jar-with-dependencies.jar edu.umass.ciir.kbbridge.ContextFeatureExtractor "
      //  sb append " /work1/allan/jdalton/tacco/scripts/runEntityLinker.sh "

      // input query
      sb append docSet.mkString(",")
      sb append " /work1/allan/jdalton/factorie-kbbridge-plugin/tac-nlp-annotations-factkb1"
      sb append " ./tac-context-features"

      // println(sb.toString)
      p.println(sb.toString)
    }
    p.close()
  }

}

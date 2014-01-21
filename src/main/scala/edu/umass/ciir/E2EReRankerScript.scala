package edu.umass.ciir.kbbridge

import edu.umass.ciir.kbbridge.tac.TacQueryUtil
import java.io.{PrintWriter, File}

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 9/16/13
 * Time: 12:06 PM
 * To change this template use File | Settings | File Templates.
 */
object E2EReRankerScript extends App {

  val allqueries = TacQueryUtil.queriesByYear()("2013")._2
  //val queries2013 = allqueries("2009")._2 ++ allqueries("2010eval")._2 ++ allqueries("2011")._2 ++ allqueries("2012")._2++ allqueries("2013")._2
  //val queries2013 = (allqueries.map(_._2)).flatten
  val querySet = allqueries.map(q => q.mentionId).toSet
  val docSet = allqueries.map(q => q.docId).toSet

  println("queries: " + querySet.size + " docs: " + docSet.size)
  writeAnnotationScript(querySet)

  def writeAnnotationScript(docsRequiringAnnotation: Iterable[String]) = {
    val outputFile = new File("./scripts/rerank-e2e-2013")
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

      sb append " java -Xmx8G -cp /work1/allan/jdalton/kbbridge/target/kbbridge-0.1-jar-with-dependencies.jar edu.umass.ciir.kbbridge.SecondPassE2E "
      //  sb append " /work1/allan/jdalton/tacco/scripts/runEntityLinker.sh "

      // input query
      sb append docSet.mkString(",")
      sb append " /work1/allan/jdalton/factorie-kbbridge-plugin/tac-nlp-annotations-2013-factkb1 /work1/allan/jdalton/factorie-kbbridge-plugin/tac-singleMention-annotations-2013-qvm_local /work1/allan/jdalton/kbbridge/data/ltr/models/tac_allqueries_e2e_mart /work1/allan/jdalton/factorie-kbbridge-plugin/tac-e2e-2013-reranked-qvm_local"
    //  sb append " /work1/allan/jdalton/factorie-kbbridge-plugin/tac-e2e-2013-reranked"

      // println(sb.toString)
      p.println(sb.toString)
    }
    p.close()
  }


}

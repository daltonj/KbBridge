package edu.umass.ciir.kbbridge

import java.io.{PrintWriter, File}
import edu.umass.ciir.kbbridge.tac.TacQueryUtil

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 7/22/13
 * Time: 1:27 PM
 */
object TacMentionLinkerScript extends App {

  val allqueries = TacQueryUtil.queriesByYear()("2012")._2
  //val queries2013 = allqueries("2009")._2 ++ allqueries("2010eval")._2 ++ allqueries("2011")._2 ++ allqueries("2012")._2++ allqueries("2013")._2
  //val queries2013 = (allqueries.map(_._2)).flatten
  val querySet = allqueries.map(q => q.mentionId).toSet
  val docSet = allqueries.map(q => q.docId).toSet

  println("queries: " + querySet.size + " docs: " + docSet.size)
  writeAnnotationScript(querySet)

  def writeAnnotationScript(docsRequiringAnnotation: Iterable[String]) = {
    val outputFile = new File("./scripts/annotate-singleMentions-2012-qvm_local")
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

      sb append "qsub -b y " + "-l mem_free=6G -l mem_token=6G" + " -cwd -o ./out/"
      sb append docSet.head
      sb append " -e ./err/"
      sb append docSet.head

      sb append " java -server -Xmx6G -Dfile.encoding=utf-8 -cp /work1/allan/jdalton/factorie-kbbridge-plugin/target/factorie-kbbridge-1.0-SNAPSHOT-jar-with-dependencies.jar cc.factorie.app.nlp.el.TacLinkingMain "
      //  sb append " /work1/allan/jdalton/tacco/scripts/runEntityLinker.sh "

      // input query
      sb append docSet.mkString(",")
      sb append " /work1/allan/jdalton/entity-linking/tac-source2013-g34"
      sb append " ./tac-singleMention-annotations-2012-qvm_local"

      // println(sb.toString)
      p.println(sb.toString)
    }
    p.close()
  }

}

package edu.umass.ciir.kbbridge

import java.io.{PrintWriter, File}
import edu.umass.ciir.kbbridge.tac.TacQueryUtil
import edu.umass.ciir.kbbridge.trec.TrecRunWriter

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 7/22/13
 * Time: 1:27 PM
 */
object Run2LinkerScript extends App {

  val allqueries = TacQueryUtil.queriesByYear
  val filtered = (allqueries("2012")._2.map(_.mentionId) ++ allqueries("2013")._2.map(_.mentionId)).toSet
  val results = RunFileLoader.readRunFileWithQuery(new File(args(0)), 5)

  val filteredQueries = results.filterKeys(filtered)
  val uniqDocs = filteredQueries.values.flatten.map(_.documentName).toSet

  println(" docs: " + uniqDocs.size)
  writeAnnotationScript(uniqDocs)

  def writeAnnotationScript(docsRequiringAnnotation: Iterable[String]) = {
    val outputFile = new File("./scripts/annotate-expansionDocs-10-swarm-factkb1")
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
      sb append " ./tac-expansion-docs-annotations-factkb1"

      // println(sb.toString)
      p.println(sb.toString)
    }
    p.close()
  }

}

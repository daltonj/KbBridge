package edu.umass.ciir.kbbridge.tac

import java.io.{PrintWriter, File}
import edu.umass.ciir.kbbridge.eval.TacLinkingEvaluator

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 9/11/13
 * Time: 3:27 PM
 * To change this template use File | Settings | File Templates.
 */
object TacLinkerEval extends App {

  val outputEvalDir = "./" + args(1) + "/" + args(2)
  val outputDirFile = new File(outputEvalDir)
  (outputDirFile.mkdirs())

  // add tac specific output dir
  val tacOutputDir = outputEvalDir + "/tac"
  val rawDir = new File(tacOutputDir)
  (rawDir.mkdirs())

  eval()

  def eval() {
    val querySet = TacQueryUtil.allQueries
    val queriesByYear = TacQueryUtil.queriesByYear.filterNot(_._1 equals "2013")
    println("query stats:")
    println("total queries: " + querySet.size)
    val nonNil =  querySet.filterNot(q => q.isNilQuery)
    println("total non-nil queries: " +nonNil.size)
    val summaryWriter = new PrintWriter(outputEvalDir + "/summary")

    for ((year, (annoFile, queries)) <- queriesByYear) {
      TacLinkingEvaluator.clearJudgments()
      TacLinkingEvaluator.loadGoldStandardFile(annoFile)
      println("Results for : " + year)
      val testQueries = queries.filter(q => querySet contains q)
      println("queries: " + testQueries.size)
      val nonNil =  testQueries.filterNot(q => q.isNilQuery)
      println("test non-nil queries: " + nonNil.size)

      var numNonMatches = 0
      val links = for (q <- testQueries) yield {
        val doc = q.docId
        val file = new File(args(0) + File.separatorChar + doc + ".xml")
        if (file.exists()) {
        //  val links = entityLinks(doc, file)
        }
      }
    }
  }




}

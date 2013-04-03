package edu.umass.ciir.kbbridge.eval

import collection.mutable.HashMap
import java.io.PrintWriter
import edu.umass.ciir.kbbridge.data.EntityMention
import edu.umass.ciir.kbbridge.tac.{TacQueryUtil}
import TacLinkingEvaluator.EvalResults

object TacMetricsCalculator {

  case class LinkerQueryPrediction (query: EntityMention, tacId: Option[Seq[String]], wikiTitle: Option[String], score: Double = 0.0)

  def evaluateMicroAverageAccuracy(predictions: Seq[LinkerQueryPrediction], outputFilePrefix: String) {
    val finalStrings = new HashMap[String, (Seq[String], Double)]()
    for (p <- predictions) {
      var tacId = p.tacId.getOrElse(List("NIL"))
      finalStrings += (p.query.mentionId ->(tacId, p.score))
    }
    val writer = new PrintWriter(outputFilePrefix + ".tac_accuracy")

    val predictedQueryStrings = predictions.map(p => p.query.mentionId).toSet

    // all years
    val alltestQueries = TacQueryUtil.selectEvenOddSplitQueries()._2


    val yearQueries = alltestQueries.filter(q => TacLinkingEvaluator.allQueries.keySet contains q.mentionId)
    val nonNilQueries = yearQueries.filterNot(_.isNilQuery)
    val nilQueries = yearQueries.filter(_.isNilQuery)

    val accOnAll = TacLinkingEvaluator.microAverageAccuracy(yearQueries.map(p => p.mentionId), finalStrings, writer, "all")
    val accOnNonNil = TacLinkingEvaluator.microAverageAccuracy(nonNilQueries.map(p => p.mentionId), finalStrings, writer, "nonNil")
    val accOnNil = TacLinkingEvaluator.microAverageAccuracy(nilQueries.map(p => p.mentionId), finalStrings, writer, "nil")
    val nilAcc = TacLinkingEvaluator.nilAccuracy(yearQueries.map(p => p.mentionId), finalStrings, writer, "nilAcc")
    val result = EvalResults(accOnAll = accOnAll, accOnNonNil = accOnNonNil, accOnNil = accOnNil, nilAcc = nilAcc)
    println(result)
    writer.close()
  }

}
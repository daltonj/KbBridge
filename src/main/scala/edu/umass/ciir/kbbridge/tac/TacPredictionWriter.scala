package edu.umass.ciir.kbbridge.tac

import edu.umass.ciir.kbbridge.data.EntityMention
import java.io.{File, PrintWriter}
import edu.umass.ciir.kbbridge.eval.TacMetricsCalculator.LinkerQueryPrediction

/**
 * User: jdalton
 * Date: 4/3/13
 */
object TacPredictionWriter {

  def removeFileIfExists: Boolean = true

  def writePredicted(predictions: Seq[LinkerQueryPrediction], filename: String, maxScore:Double=Double.NegativeInfinity, minScore:Double = Double.NegativeInfinity) {

    val f = new File(filename)
    if (f.exists()) {
      if (removeFileIfExists) f.delete()
      else {
        throw new RuntimeException("File " + f.getAbsolutePath + " already exists -- abort.")
      }
    }

    val writer = new PrintWriter(filename)
    for (p <- predictions) {
      val mentionId = p.query.mentionId
      val tacId = p.tacId.get.head
      val entityType = p.query.entityType
      var normalizedScore = maxMinNorm(p.score, -5, maxScore)
      if (normalizedScore <= 0.001) {
        normalizedScore = 0.001
      }
      writer.println(Seq(mentionId,tacId,normalizedScore).mkString("\t"))
    }
    writer.close()
  }
  def maxMinNorm(score:Double, min:Double, max:Double): Double = {
    val range = max - min
    (score-min)/range
  }

}

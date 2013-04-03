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

  def writePredicted(predictions: Seq[LinkerQueryPrediction], filename: String) {

    val f = new File(filename)
    if (f.exists()) {
      if (removeFileIfExists) f.delete()
      else {
        throw new RuntimeException("File " + f.getAbsolutePath + " already exists -- abort.")
      }
    }

    val writer = new PrintWriter(filename)
    for (p <- predictions) {
      writer.println(p.query.mentionId + "\t" + p.tacId.get + "\t" + p.query.entityType)
    }
    writer.close()
  }

}

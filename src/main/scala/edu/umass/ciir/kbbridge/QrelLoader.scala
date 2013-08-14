package edu.umass.ciir.kbbridge

import io.Source
import collection.mutable.ListBuffer

/**
 * User: jdalton
 * Date: 6/6/13
 */

case class Judgment(var queryId: QueryId, var objectId: String, var relevanceLevel: Int)

object QrelLoader {

  def fromTrec(src: String, useBinary: Boolean = false): QueryJudgmentSet = {
    val reader = Source.fromFile(src).bufferedReader()

    val judgments =  new ListBuffer[Judgment]()
    while(reader.ready) {
      val line = reader.readLine
      val columns = line.split("\\s+")
      val queryId = columns(0)
      val docId = columns(2)
      val relevance = columns(3).toInt
      judgments += Judgment(queryId, docId, relevance)
    }
    reader.close
    val map = judgments.groupBy(j => j.queryId)
    map
  }

}

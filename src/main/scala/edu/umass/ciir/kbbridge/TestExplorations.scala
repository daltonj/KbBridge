package edu.umass.ciir.kbbridge

import features.FileFeatureLoader
import tac.TacQueryUtil
import scala.collection.JavaConversions._

/**
 * User: jdalton
 * Date: 5/15/13
 */
object TestExplorations extends App {

  val queries = TacQueryUtil.allQueries()

  val filtered = queries.filterNot(q => q.isNilQuery).filter(q => q.groundTruthWikiTitle.get contains "Northampton")
  val p = new java.io.PrintWriter("./nohoqueries")
  val featureMap = FileFeatureLoader.loadProtobufDataForQueries(filtered).map(f => f.getMention.getMentionId -> f).toMap
  for (q <- filtered) {
    println(q)
    val features = featureMap(q.mentionId)
    p.println("DOCID:" + q.docId)
    p.println(q)
    p.println(q.groundTruthWikiTitle)
    val candidates = features.getMention.getCandidatesList take 10
    for (c <- candidates) {
      p.println(c.getWikipediaTitle + " " + c.getRank + " " + c.getScore)
    }

    p.println(q.fullText)

  }
  //(q => println(q +" " + q.groundTruthWikiTitle.get))
  p.close()
}

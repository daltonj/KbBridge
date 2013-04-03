package edu.umass.ciir.kbbridge.nil

import scala.collection.mutable.ListBuffer
import collection.mutable.HashMap
import edu.umass.ciir.kbbridge.eval.TacMetricsCalculator.LinkerQueryPrediction

object NilClusterer {

  var lastNil = 0
  var queryNameToNilNumber = new HashMap[String, Int]()

  def clusterNils(predictions: Seq[LinkerQueryPrediction]): Seq[LinkerQueryPrediction] = {

    val finalResults = new ListBuffer[LinkerQueryPrediction]

    for (p <- predictions) {
      val aid = p.tacId match {
        case None => {
          if (p.wikiTitle.isDefined && p.wikiTitle.get.length() > 0) {
            // first use nilcluster
            List("NIL" + nextNilNumber("##" + p.wikiTitle.get))
          } else {
            // if no nilcluster given, use the query name
            List("NIL" + nextNilNumber("--" + p.query.mentionId))
            //"NIL"+nextNilNumber("--"+p.queryName)
          }
        }
        case Some(an) => an
      }
      finalResults += new LinkerQueryPrediction(p.query, Some(aid), p.wikiTitle, p.score)
    }
    finalResults
  }

  def nextNilNumber(qname: String): String = {
    val oldNil = queryNameToNilNumber.get(qname)
    oldNil match {
      case None => {
        lastNil += 1

        queryNameToNilNumber += (qname -> lastNil)
        "%04d".format(lastNil)
      }
      case Some(recycledNil) => {
        "%04d".format(recycledNil)
      }
    }
  }

}
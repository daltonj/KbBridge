package edu.umass.ciir.kbbridge.search

import java.io.File
import scala.collection.JavaConversions._
import org.lemurproject.galago.core.retrieval.{ScoredDocument, RetrievalFactory}
import org.lemurproject.galago.tupleflow.Parameters

/**
 * User: jdalton
 * Date: 3/29/13
 */
//class KnowledgeBaseSearcher {
//
//  val retrieval = RetrievalFactory.instance(parameters);
//
//  def runQueries(queryFile:File, baseParams: Parameters, runFile:String, qrelFile:String) : HashMap[String, Seq[ScoredDocument]] = {
//    val p = Parameters.parse(queryFile);
//    p.set("startAt", 0)
//    p.set("resultCount", 10)
//    p.set("retrievalGroup", "all")
//    p.set("requested", 4000)
//    // p.set("mu", 500)
//    p.copyFrom(baseParams)
//  }
//
//}

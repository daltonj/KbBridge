package edu.umass.ciir.kbbridge.kb2text

import org.lemurproject.galago.core.retrieval.ScoredDocument
import edu.umass.ciir.kbbridge.search.{GalagoDoc2RetrievedText, GalagoRetrieval, DocumentBridgeMap}
import edu.umass.ciir.kbbridge.data.repr.EntityRepr
import edu.umass.ciir.kbbridge.data.{ScoredBridgeDocument, DocumentProvider, BridgeDocument}
import org.lemurproject.galago.tupleflow.Parameters

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 5:20 PM
 */
object GalagoDoc2BridgeDoc {

  def createDocConverter(galago:GalagoRetrieval):GalagoDoc2RetrievedText = {
    new GalagoDoc2RetrievedText(galago)
  }



}

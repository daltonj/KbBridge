package edu.umass.ciir.kbbridge.search

import org.lemurproject.galago.core.retrieval.ScoredDocument
import edu.umass.ciir.kbbridge.data.{ScoredBridgeDocument, GalagoBridgeDocumentWrapper, GalagoBridgeDocument}
import edu.umass.ciir.kbbridge.util.SeqTools
import org.lemurproject.galago.tupleflow.Parameters

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 5:20 PM
 */
class GalagoDoc2RetrievedText(galago: GalagoRetrieval) {

  def galagoResultToRetrievedText(seq: Seq[ScoredDocument], params:Option[Parameters]=None): Seq[GalagoBridgeDocument] = {
    val name2sd = seq.map(elem => (elem.documentName, elem))
    val name2doc = galago.getDocuments(seq.map(_.documentName), params)

    val merged = SeqTools.alignMaps(name2sd, name2doc)
    for ((key, (sd, galagoDoc)) <- merged) yield {
      galagoMergedDocToRetrievedText(sd, galagoDoc)
    }

  }

  def galagoMergedDocToRetrievedText(sd: ScoredDocument, galagoDoc: org.lemurproject.galago.core.parse.Document): GalagoBridgeDocument = {
    val docname = sd.documentName
    val score = sd.score
    val rank = sd.rank


    new GalagoBridgeDocumentWrapper(
      documentname = docname
      , rawScore = Some(score)
      , rank = Some(rank)
      , galagoDocument = Some(galagoDoc)
    ).asInstanceOf[GalagoBridgeDocument]

  }
}

package edu.umass.ciir.kbbridge.search

import org.lemurproject.galago.core.retrieval.ScoredDocument
import edu.umass.ciir.kbbridge.data.GalagoDocument
import edu.umass.ciir.kbbridge.util.SeqTools

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 5:20 PM
 */
class GalagoDoc2RetrievedText(galago: GalagoRetrieval) {

  def galagoResultToRetrievedText(seq: Seq[ScoredDocument]): Seq[GalagoDocument] = {
    val name2sd = seq.map(elem => (elem.documentName, elem))
    val name2doc = galago.getDocuments(seq.map(_.documentName))

    val merged = SeqTools.alignMaps(name2sd, name2doc)
    for ((key, (sd, galagoDoc)) <- merged) yield {
      galagoMergedDocToRetrievedText(sd, galagoDoc)
    }

  }

  def galagoMergedDocToRetrievedText(sd: ScoredDocument, galagoDoc: org.lemurproject.galago.core.parse.Document): GalagoDocument = {
    val docname = sd.documentName
    val score = sd.score
    val rank = sd.rank


    new GalagoDocument(
      documentname = docname
      , score = score
      , rank = rank
      , galagoDocument = galagoDoc
    )

  }
}

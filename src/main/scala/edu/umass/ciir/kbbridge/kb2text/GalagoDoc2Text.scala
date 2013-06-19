package edu.umass.ciir.kbbridge.kb2text

import org.lemurproject.galago.core.retrieval.ScoredDocument
import edu.umass.ciir.kbbridge.data.{IdMap, ScoredWikipediaEntity}
import org.lemurproject.galago.core.parse.Document
import edu.umass.ciir.kbbridge.search.DocumentBridgeMap
import edu.umass.ciir.kbbridge.data.repr.EntityRepr

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 5:20 PM
 */
object GalagoDoc2Text {



  def galagoResultsToDocForReverseLinking(docs: Seq[ScoredDocument],originalQueryEntityRepr:EntityRepr): Seq[TrecKbaDocumentForReverseLinking] = {
    for (d <- docs) yield {
      galagoResultToDocForReverseLinking(d, originalQueryEntityRepr)
    }
  }
  def galagoResultToDocForReverseLinking(sd: ScoredDocument, originalQueryEntityRepr:EntityRepr): TrecKbaDocumentForReverseLinking = {

    val pulledDoc = DocumentBridgeMap.getDefaultDocumentProvider.getBridgeDocument(sd.documentName)
    val nameVariants = originalQueryEntityRepr.nameVariants.map(_._1)
    val originalQueryName = originalQueryEntityRepr.entityName

    val score = sd.score
    val rank = sd.rank

    new TrecKbaDocumentForReverseLinking(pulledDoc, originalQueryName, nameVariants, Some(originalQueryEntityRepr), score=Some(score), rank = Some(rank))
  }


  def galagoResultsToTrecKbaDoc(docs: Seq[ScoredDocument],originalQueryEntityRepr:EntityRepr): Seq[TrecKbaDocument] = {
    for (d <- docs) yield {
      galagoResultToTrecKbaDocument(d, originalQueryEntityRepr)
    }
  }
  def galagoResultToTrecKbaDocument(sd: ScoredDocument, originalQueryEntityRepr:EntityRepr): TrecKbaDocument = {

    val score = sd.score
    val rank = sd.rank

    new TrecKbaDocument(sd.documentName, topicId = originalQueryEntityRepr.entityName, rank = rank, rawScore=Some(score))
  }


}

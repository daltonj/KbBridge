package edu.umass.ciir.kbbridge.text2kb

import edu.umass.ciir.kbbridge.search.{GalagoRetrieval, EntityRetrievalWeighting, EntityReprRetrieval, DocumentBridgeMap}
import edu.umass.ciir.kbbridge.data.{DocumentProvider, ScoredWikipediaEntity, EntityMention}
import org.lemurproject.galago.core.retrieval.ScoredDocument
import edu.umass.ciir.kbbridge.util.ConfInfo

/**
 * User: jdalton
 * Date: 6/10/13
 */

object KnowledgeBaseCandidateGenerator {

  def apply() : KnowledgeBaseCandidateGenerator = {
    val galago = new GalagoRetrieval(
      jsonConfigFile= ConfInfo.galagoKbJsonParameterFile,
      galagoUseLocalIndex = true
    )

    new KnowledgeBaseCandidateGenerator(galago, EntityRetrievalWeighting(0.5, 0.5, 0.0, 0.0), QVTextEntityRepr)
  }

}

class KnowledgeBaseCandidateGenerator(val galago : GalagoRetrieval, val weighting: EntityRetrievalWeighting, val reprGenerator :TextEntityReprGenerator) {
  val candidateGenerator = new EntityReprRetrieval(galago, weighting)

  def retrieveCandidates(mention: EntityMention, numCandidates: Int) : Seq[ScoredWikipediaEntity] = {
    val retrievedCands = candidateGenerator.search(reprGenerator.createEntityRepr(mention), numCandidates)
    val cands = GalagoDoc2WikipediaEntity.galagoResultToWikipediaEntities(retrievedCands)
    cands
  }

}

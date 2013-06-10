package edu.umass.ciir.kbbridge.text2kb

import edu.umass.ciir.kbbridge.search.{GalagoRetrieval, EntityRetrievalWeighting, EntityReprRetrieval, RetrievalMap}
import edu.umass.ciir.kbbridge.data.{ScoredWikipediaEntity, EntityMention}
import org.lemurproject.galago.core.retrieval.ScoredDocument

/**
 * User: jdalton
 * Date: 6/10/13
 */

object KnowledgeBaseCandidateGenerator {

  def apply() : KnowledgeBaseCandidateGenerator = {
    new KnowledgeBaseCandidateGenerator(RetrievalMap.getSearcher, EntityRetrievalWeighting(0.5, 0.5, 0.0, 0.0))
  }

}

class KnowledgeBaseCandidateGenerator(val galago : GalagoRetrieval, val weighting: EntityRetrievalWeighting) {
  val reprGenerator = new TextEntityReprGenerator()
  val candidateGenerator = new EntityReprRetrieval(galago, weighting)

  def retrieveCandidates(mention: EntityMention, numCandidates: Int) : Seq[ScoredWikipediaEntity] = {
    val retrievedCands = candidateGenerator.search(reprGenerator.createQVEntityRepr(mention), numCandidates)
    val cands = GalagoDoc2WikipediaEntity.galagoResultToWikipediaEntities(retrievedCands)
    cands
  }

}

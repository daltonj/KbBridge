package edu.umass.ciir.kbbridge

import data.{ScoredWikipediaEntity, EntityMention}
import text2kb.KnowledgeBaseCandidateGenerator
import util.{ConfInfo, KbBridgeProperties}

/**
 * User: jdalton
 * Date: 6/12/13
 */
object SimpleEntityLinker {

  lazy val candidateGenerator = KnowledgeBaseCandidateGenerator()
  val reranker = new RankLibReranker(KbBridgeProperties.rankerModelFile)

  def rankKbs(mention: EntityMention, numberOfCandidates: Int = ConfInfo.maxCandidates) : Seq[ScoredWikipediaEntity] = {
    println("Fetching candidates for mention: " + mention.mentionId + " d:" + mention.docId + " name:" + mention.entityName)
    val candidates = candidateGenerator.retrieveCandidates(mention, numberOfCandidates)
    val rerankedResults = reranker.rerankCandidatesGenerateFeatures(mention, candidates)
    rerankedResults
  }

}

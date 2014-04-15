package edu.umass.ciir.kbbridge


import data.{TacEntityMention, ScoredWikipediaEntity, EntityMention}
import features.{EntityFeaturesToSvmConverter, Mention2EntityFeatureHasher}


import scala.collection.mutable.ListBuffer

import ciir.umass.edu.learning.{RankerFactory, DenseDataPoint}
import collection.mutable
import search.DocumentBridgeMap
import edu.umass.ciir.kbbridge.serial.EntityMentionProtos.{ScoredWikipediaEntityFeatures, TacEntityMentionLinkerFeatures}
import edu.umass.ciir.kbbridge.util.{WikiLinkExtractor, ConfInfo}
import scala.collection.JavaConversions._
import org.lemurproject.galago.tupleflow.Parameters


class RankLibReranker(rankerModelFile: String, featureConfig:Seq[String] = ConfInfo.rankingFeatures, domainFile:String = "./data/ltr/domainMap") {

  val ltrModel = new RankerFactory().loadRanker(rankerModelFile)
  val svmConverter = new EntityFeaturesToSvmConverter(domainFile)
  def rerankCandidatesGenerateFeatures(mention: EntityMention, entities: Seq[ScoredWikipediaEntity]): Seq[ScoredWikipediaEntity] = {

    var scoredDocuments = new ListBuffer[ScoredWikipediaEntity]

    //    case "query_only" => m2eFeatures.values.flatMap(_.keys).filter(f => (f.startsWith("query"))).toSet
    //   case "query_nv" => m2eFeatures.values.flatMap(_.keys).filter(f => (f.startsWith("query") || f.startsWith("nameVariants"))).toSet
    //   case "all_local" => m2eFeatures.values.flatMap(_.keys).toSet
    //   case "all" => m2eFeatures.values.flatMap(_.keys).toSet ++ featureComponents
    // }

    //  val rerankedComponents = GalagoOnlyRanker.reRankUsingScoreComponents(queryComponents, rankingComponentWeights)

    val candidates = entities.sortBy(-_.score)

    //        if (useTacOnly) {
    //          candidates = candidates.filter(_.data.asInstanceOf[GalagoWikipediaEntity[TacELQueryImpl]].tacIds.length > 0)
    //        }

    val candsWithRank = candidates.zipWithIndex
    for ((entity, rank) <- candsWithRank) {
      // now for the features
      val docParams = new Parameters()

      docParams.set("text", true)
      docParams.set("tokenize", true)
      docParams.set("metadata", true)
      val galagoDoc = DocumentBridgeMap.getKbDocumentProvider.getDocument(entity.wikipediaTitle,Some(docParams))
      entity.document = galagoDoc

      if (featureConfig contains "e2e") {
        entity.incomingLinks = WikiLinkExtractor.simpleExtractorNoContext(galagoDoc).map(a => a.destination).toSet
        entity.outgoingLinks = galagoDoc.metadata.getOrElse("srcInlinks", "").split("\\s+").toSet
        entity.combinedLinks = entity.incomingLinks ++ entity.outgoingLinks
      }

      val m2eFeatures = Mention2EntityFeatureHasher.featuresAsMap(featureConfig, mention, entity, entities)
      val svmString = svmConverter.entityToSvmFormat(mention, entity, m2eFeatures)
      try {
      val featureData = new DenseDataPoint(svmString)
      val score = ltrModel.eval(featureData)
     //println(score)
      scoredDocuments += new ScoredWikipediaEntity(entity.wikipediaTitle, entity.wikipediaId, score, (rank + 1), featureMap = Some(m2eFeatures))
      } catch {
        case ex: Exception => println(ex.getMessage + " data line:\n" + svmString)
      }
      entity.document = null
    }

    //println(query)
    val sorted = scoredDocuments.sortBy(d => (-d.score, d.rank))
    for ((result, idx) <- sorted.zipWithIndex) {
      result.rank = (idx + 1)
      //println(result.rank + " " + result.documentName + " " + result.score)

    }
    //sorted.map(e => println(e.documentName + " " + e.score))
    sorted.toSeq
  }


  def rerankCandidatesWithFeatures(mention: EntityMention, entities: Seq[ScoredWikipediaEntityFeatures]): Seq[ScoredWikipediaEntity] = {

    var scoredDocuments = new ListBuffer[ScoredWikipediaEntity]

    //    case "query_only" => m2eFeatures.values.flatMap(_.keys).filter(f => (f.startsWith("query"))).toSet
    //   case "query_nv" => m2eFeatures.values.flatMap(_.keys).filter(f => (f.startsWith("query") || f.startsWith("nameVariants"))).toSet
    //   case "all_local" => m2eFeatures.values.flatMap(_.keys).toSet
    //   case "all" => m2eFeatures.values.flatMap(_.keys).toSet ++ featureComponents
    // }

    //  val rerankedComponents = GalagoOnlyRanker.reRankUsingScoreComponents(queryComponents, rankingComponentWeights)

    //   val candidates = entities.sortBy(- _.getScore)

    //        if (useTacOnly) {
    //          candidates = candidates.filter(_.data.asInstanceOf[GalagoWikipediaEntity[TacELQueryImpl]].tacIds.length > 0)
    //        }
    //    val candsWithRank = candidates.zipWithIndex
    for (entity <- entities) {
      // now for the features
      val m2eFeatures = entity.getRankingFeaturesList.map(f => f.getKey -> f.getValue).toMap
      val entityCandidate = new ScoredWikipediaEntity(entity.getWikipediaTitle, entity.getWikipediaId, entity.getScore, entity.getRank)
      val svmString = svmConverter.entityToSvmFormat(mention, entityCandidate, m2eFeatures)
      val featureData = new DenseDataPoint(svmString)
      val score = ltrModel.eval(featureData)
      //  println(score)
      scoredDocuments += new ScoredWikipediaEntity(entity.getWikipediaTitle, entity.getWikipediaId, score, entity.getRank)

    }

    //println(query)
    val sorted = scoredDocuments.sortBy(d => (-d.score, d.rank))
    for ((result, idx) <- sorted.zipWithIndex) {
      result.rank = (idx + 1)
      //println(result.rank + " " + result.documentName + " " + result.score)

    }
    //sorted.map(e => println(e.documentName + " " + e.score))
    sorted.toSeq
  }

  def rerankMentionBatchWithFeatures(mentions: Seq[TacEntityMentionLinkerFeatures]): Map[String, Seq[ScoredWikipediaEntity]] = {

    val resultMap = new mutable.HashMap[String, Seq[ScoredWikipediaEntity]]

    for (mention <- mentions) {
      val mentionFeatures = mention.getMention
      val tacEntityMention = new TacEntityMention(mentionFeatures.getSourceDocId, mentionFeatures.getEntityType, mentionFeatures.getMentionId, mentionFeatures.getEntityName, Seq(), mention.getNodeId, mention.getGroundTruthWikiTitle, Seq())
      val reranked = rerankCandidatesWithFeatures(tacEntityMention, mentionFeatures.getCandidatesList)
      resultMap += tacEntityMention.mentionId -> reranked
    }
    resultMap.toMap
  }


}

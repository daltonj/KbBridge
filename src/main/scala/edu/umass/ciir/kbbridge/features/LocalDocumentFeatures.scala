package edu.umass.ciir.kbbridge.features

import scala.collection.JavaConversions._
import edu.umass.ciir.models._
import org.lemurproject.galago.core.parse.Document
import scala.collection.mutable.HashMap
import util.{SetMeasures, LanguageModelFeatures}
import edu.umass.ciir.kbbridge.search.{DocumentBridgeMap}
import edu.umass.ciir.kbbridge.nlp.{TextNormalizer}
import edu.umass.ciir.kbbridge.util.{WikiLinkExtractor}
import edu.umass.ciir.kbbridge.data.{DocumentProvider, SimpleEntityMention, EntityMention, WikipediaEntity}
import edu.umass.ciir.kbbridge.text2kb.{GalagoDoc2WikipediaEntity}

trait LocalDocumentFeatures extends FeatureGenerator {

  val featurePrefix = "localTextSimilarity"

  val cosineFeature = "cosineFeature"
  val kldivergenceFeature = "klfFeature"
  val jsdivergenceFeature = "jsdivergenceFeature"
  val jaccardFeature = "jaccardFeature"

  val nerIntersect = "nerIntersect"
  val nerMisses = "nerMisses"
  val nerDice = "nerDice"
  val nerJaccard = "nerJaccard"

  val searcher = DocumentProvider

  def contextSimFeatures(mContext: String, eContext: String, contextType: String) {
    val textSimFeatures = LanguageModelFeatures.computeLmSimilarity(mContext, eContext, true);
    addValueFeature(featurePrefix, cosineFeature + "-" + contextType, textSimFeatures.cosine)
    addValueFeature(featurePrefix, kldivergenceFeature + "-" + contextType, textSimFeatures.klDivergence)
    addValueFeature(featurePrefix, jsdivergenceFeature + "-" + contextType, textSimFeatures.jsDivergence)
    addValueFeature(featurePrefix, jaccardFeature + "-" + contextType, textSimFeatures.jaccard)
  }


  def nerContextSimilarity(mentionNers: Seq[String], galagoEntityDoc: Document) = {

    val featureValues = HashMap[String, Double]()

    val normalizedMentionNers = mentionNers.map(name => TextNormalizer.normalizeText(name))
    val mentionEntityLm = new LanguageModel(1)
    mentionEntityLm.addDocument(normalizedMentionNers, false)


    // note: we use an n-gram size of 6 here to cover our entity lookups
    val entityTextLm = new LanguageModel(6)
    entityTextLm.addDocument(galagoEntityDoc, false)

    val backgroundLm = new LanguageModel(1)
    backgroundLm.setCollectionFrequency(6060567681L);

    var modelScorer = new KLDivergenceSimilarity(backgroundLm, 1000)
    var klDivergenceScore = modelScorer.calculateSimilarity(mentionEntityLm, entityTextLm, false).getSimilarity()
    if (klDivergenceScore > 100) {
      klDivergenceScore = 100
    }
    featureValues += ("nerContextSimilarityKLDivergence" -> klDivergenceScore)

    var numEntityMatches = 0
    for (ner <- normalizedMentionNers) {
      val te = entityTextLm.getTermEntry(ner)
      if (te != null) {
        numEntityMatches += 1
      }
    }


    val entityMatchProb = {
      if (normalizedMentionNers.size() <= 0) {
        0.0
      } else {
        numEntityMatches.toDouble / normalizedMentionNers.size()
      }
    }
    featureValues += ("nerContextSimilarityMatchProb" -> entityMatchProb)
    featureValues

  }


  def entityLinkBasedFeatures(mentionNers: Seq[String], galagoEntityDoc: Document) = {

    val featureValues = HashMap[String, Double]()

    val normalizedMentionNers = mentionNers.map(name => TextNormalizer.normalizeText(name))
    val mentionEntityLm = new LanguageModel(1)
    mentionEntityLm.addDocument(normalizedMentionNers, false)

    val outlinks = WikiLinkExtractor.extractLinks(galagoEntityDoc).map(a => TextNormalizer.normalizeText(a.destination)).toSet
    //println("num outlinks: " + outlinks.size())

    val inlinks = galagoEntityDoc.metadata.getOrElse("srcInlinks", "").split("\\s+").map(in => TextNormalizer.normalizeText(in)).toSet
    //println("num inlinks"  + inlinks.size())

    val entityLinkLm = new LanguageModel(1)
    entityLinkLm.addDocument(outlinks, false)
    entityLinkLm.addDocument(inlinks, false)

    val backgroundLm = new LanguageModel(1)
    backgroundLm.setCollectionFrequency(6060567681L);

    var modelScorer = new KLDivergenceSimilarity(backgroundLm, 100)
    var klDivergenceScore = modelScorer.calculateSimilarity(mentionEntityLm, entityLinkLm, false).getSimilarity()
    if (klDivergenceScore > 100) {
      klDivergenceScore = 100
    }
    featureValues += ("entityLinkKlDivergence" -> klDivergenceScore)

    var jsScorer = new JS_KLDivergenceSimilarity(backgroundLm, 100)
    var jsDivergenceScore = jsScorer.calculateSimilarity(mentionEntityLm, entityLinkLm, false).getSimilarity()

    if (jsDivergenceScore > 100) {
      jsDivergenceScore = 100
    }
    featureValues += ("entityLinkJsDivergence" -> jsDivergenceScore)

    val linkSet = outlinks ++ inlinks
    val nerSetSimilarity = SetMeasures(normalizedMentionNers.toSet, linkSet)
    featureValues += (nerIntersect -> 1.0 * nerSetSimilarity.intersect)
    featureValues += (nerMisses -> 1.0 * nerSetSimilarity.missNum)
    featureValues += (nerDice -> nerSetSimilarity.diceCorrect)
    featureValues += (nerJaccard -> nerSetSimilarity.jaccard)

    featureValues
  }


  def generateDocumentContextFeatures(mention: EntityMention, entity: WikipediaEntity, otherCands: Seq[WikipediaEntity]) {
    val entityFullTextAllFields = entity.document.terms.mkString(" ")
    contextSimFeatures(mention.fullText, entityFullTextAllFields, "docText")

    val documentNers = mention.nerNeighbors.map(ner => ner.text)
    val nerContextFeatures = nerContextSimilarity(documentNers, entity.document)
    mapToFeature(nerContextFeatures.toMap)

    val nerContextLinkFeatures = entityLinkBasedFeatures(documentNers, entity.document)
    mapToFeature(nerContextLinkFeatures.toMap)

  }

  def mapToFeature(features: Map[String, Double]) = {
    features.map(e => addValueFeature(featurePrefix, e._1, e._2))
  }

}

object LocalDocumentFeaturesTest {
  def main(args: Array[String]) {

    val featureMap = scala.collection.mutable.Map[String, Double]()
    val prefixSeparator = " . "

    def addFeatureCall(prefix: String, category: String, value: String) {
      featureMap += (prefix + prefixSeparator + category + "=" + value -> 1.0)
    }

    def addFeatureValueCall(prefix: String, category: String, value: Double) {
      featureMap += (prefix + prefixSeparator + category -> value)
    }

    val entity = GalagoDoc2WikipediaEntity.idToEntity("American_Automobile_Association")
    val mention = new SimpleEntityMention(docId = "eng-WL-11-174611-12978627", entityType = "ORG", mentionId = "EL_00637", entityName = "Alabama", fullText = "")
    val queryOnlyFeatures = new FeatureSetup(addFeatureCall, addFeatureValueCall) with LocalDocumentFeatures {}

    val document = DocumentBridgeMap.getKbDocumentProvider.getDocument(entity.wikipediaTitle)
    entity.document = document

    queryOnlyFeatures.generateDocumentContextFeatures(mention, entity, Seq())
    featureMap.keySet.toList.sortWith((s1, s2) => (s1 < s2)).map(p => println(p + "=" + featureMap.get(p).get))

  }
}
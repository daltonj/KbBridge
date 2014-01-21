package edu.umass.ciir.kbbridge.features

import edu.umass.ciir.kbbridge.features.util.{SetMeasures, LanguageModelFeatures}
import edu.umass.ciir.kbbridge.data.{SimpleEntityMention, ScoredWikipediaEntity, EntityMention, WikipediaEntity}
import edu.umass.ciir.kbbridge.nlp.TextNormalizer
import edu.umass.ciir.kbbridge.util.WikiLinkExtractor
import org.lemurproject.galago.core.parse.Document
import edu.umass.ciir.kbbridge.search.DocumentBridgeMap
import org.lemurproject.galago.core.retrieval.LocalRetrieval
import edu.umass.ciir.models.StopWordList
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.collection.JavaConversions._
import scala.collection.mutable
import edu.umass.ciir.kbbridge.text2kb.GalagoDoc2WikipediaEntity
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 9/12/13
 * Time: 4:50 PM
 */
trait Entity2EntityFeatures extends FeatureGenerator {

  val featurePrefix = "E2E"

  // category id set based features
  val categoryIntersect = "categoryIntersect"
  val categoryMisses = "categoryMisses"
  val categoryDiceOverlap = "categoryDiceOverlap"
  val categoryJaccardOverlap = "categoryJaccardOverlap"
  val categoryCosine = "categoryCosine"

  // category bag of words similarity
  val categoryVocabularyJaccard = "categoryVocabularyJaccard"
  val categoryVocabularyJsDivergence = "categoryVocabularyJsDivergence"
  val categoryVocabularyTfIdfCosine = "categoryVocabularyTfIDFCosine"
  val categoryVocabularySetCosine = "categoryVocabUnweightedCosine"

  // article text based features
  val cosineTextFeature = "cosineTextFeature"
  val jsdivergenceTextFeature = "jsTextdivergenceFeature"
  val jaccardTextFeature = "jaccardTextFeature"

  // link based features
  val inlinkPMI = "inlinkPMI"
  val inlinkProxPMI = "inlinkProxPMI"
  val inlinkIntersection = "inlinkIntersection"
  val inlinkJaccard = "inlinkJaccard"
  val inlinkDice = "inlinkDice"
  val inlinkNormGoogleDistance = "inlinkNormGoogleDistance"

  val outlinkDice = "outlinkDice"
  val outlinkProxPMI = "outlinkProxPMI"
  val outlinkPMI = "outlinkPMI"
  val outlinkIntersection = "outlinkIntersection"
  val outlinkJaccard = "outlinkJaccard"
  val outlinkNormGoogleDistance = "outlinkNormGoogleDistance"

  // whether the entities link to one another. (binary features)
  val isLinked = "oneLink"
  val mutualLink = "mutualLink"
  val matchingEntity = "matchingEntity"
  val matchingEntityScore = "matchingEntityScore"

  // union of outlink + inlink as context set.
  val combinedLinkIntersection = "combinedLinkIntersection"
  val combinedLinkJaccard = "combinedLinkJaccard"
  val combinedLinkPMI = "combinedLinkPMI"
  val combinedLinkProxPMI = "combinedLinkProxPMI"
  val combinedLinkDice = "combinedLinkDice"
  val combinedLinkNormGoogleDistance = "combinedLinkNormGoogleDistance"

  val containsName = "oneContainsName"
  val bothContainname = "bothContainName"

  val (collLength, numDocs) = DocumentBridgeMap.getKbRetrieval.collectionStatistics()
 // println("coll freq: " + collLength + " num docs " + numDocs)

  def textSimilarityFeatures(e1Context: String, e2Context: String, aggregateMap: HashMap[String, ListBuffer[Double]]) {
    val textSimFeatures = LanguageModelFeatures.computeLmSimilarity(e1Context, e2Context, true)
    addToAggregateMap(featurePrefix, cosineTextFeature, textSimFeatures.cosine, aggregateMap)
    addToAggregateMap(featurePrefix, jsdivergenceTextFeature, textSimFeatures.jsDivergence, aggregateMap)
    addToAggregateMap(featurePrefix, jaccardTextFeature, textSimFeatures.jaccard, aggregateMap)
  }

  def textContainmentFeatures(entity1Title: String, entity2Title: String, e1Text: String, e2Text: String, aggregateMap: HashMap[String, ListBuffer[Double]]) {

    val e1Name = TextNormalizer.normalizeText(entity1Title)
    val e2Name = TextNormalizer.normalizeText(entity2Title)

    var e2Ine1 = false
    var e1Ine2 = false


    // TODO use entity alternative names: redirects, anchors
    if (e1Text contains e2Name) {
      e2Ine1 = true
    }

    if (e2Text contains e1Name) {
      e1Ine2 = true
    }

    // either contains the other entity
    if (e2Ine1 || e1Ine2) {
      addToAggregateMap(featurePrefix, containsName, 1, aggregateMap)
    } else {
      addToAggregateMap(featurePrefix, containsName, 0, aggregateMap)
    }

    // mutual mention in both articles
    if (e2Ine1 && e1Ine2) {
      addToAggregateMap(featurePrefix, bothContainname, 1, aggregateMap)
    } else {
      addToAggregateMap(featurePrefix, bothContainname, 0, aggregateMap)
    }

  }

  def linkFeatures(entity: WikipediaEntity, galagoEnitityDoc2: Document, aggregateMap: HashMap[String, ListBuffer[Double]]) {

    // get outgoing links
    val e1outGoingLinks = entity.outgoingLinks
    val e2outGoingLinks = WikiLinkExtractor.simpleExtractorNoContext(galagoEnitityDoc2).map(a => a.destination).toSet
    val outgoingLinkMeasures = SetMeasures(e1outGoingLinks, e2outGoingLinks)
   // println("num incoming link feats: " + e1outGoingLinks.size + " " + e2outGoingLinks.size)

    addToAggregateMap(featurePrefix, outlinkProxPMI, outgoingLinkMeasures.approxPMI, aggregateMap)
    addToAggregateMap(featurePrefix, outlinkPMI, outgoingLinkMeasures.PMI(numDocs), aggregateMap)
    addToAggregateMap(featurePrefix, outlinkIntersection, outgoingLinkMeasures.intersect, aggregateMap)
    addToAggregateMap(featurePrefix, outlinkJaccard, outgoingLinkMeasures.jaccard, aggregateMap)
    addToAggregateMap(featurePrefix, outlinkDice, outgoingLinkMeasures.diceCorrect, aggregateMap)

    addToAggregateMap(featurePrefix, outlinkNormGoogleDistance, outgoingLinkMeasures.normalizedGoogleDistance(numDocs), aggregateMap)


    if (galagoEnitityDoc2.metadata == null) System.err.println(featurePrefix + " entity2.metadata is null (" + galagoEnitityDoc2.name + " keys: " + galagoEnitityDoc2.metadata.keys.mkString(", ") + ")")
    val e1incomingLinks = entity.incomingLinks
    val e2incomingLinks = galagoEnitityDoc2.metadata.getOrElse("srcInlinks", "").split("\\s+").toSet
  //  println("num incoming link feats: " + e1incomingLinks.size + " " + e2incomingLinks.size)

    val incomingLinkMeasures = SetMeasures(e1incomingLinks, e2incomingLinks)
    addToAggregateMap(featurePrefix, inlinkProxPMI, incomingLinkMeasures.approxPMI, aggregateMap)
    addToAggregateMap(featurePrefix, inlinkPMI, incomingLinkMeasures.PMI(numDocs), aggregateMap)
    addToAggregateMap(featurePrefix, inlinkIntersection, incomingLinkMeasures.intersect, aggregateMap)
    addToAggregateMap(featurePrefix, inlinkJaccard, incomingLinkMeasures.jaccard, aggregateMap)
    addToAggregateMap(featurePrefix, inlinkDice, incomingLinkMeasures.diceCorrect, aggregateMap)
    addToAggregateMap(featurePrefix, inlinkNormGoogleDistance, incomingLinkMeasures.normalizedGoogleDistance(numDocs), aggregateMap)

    val linked = e1outGoingLinks.contains(galagoEnitityDoc2.name) || e2outGoingLinks.contains(entity.wikipediaTitle)
    val mutualLinked = e1outGoingLinks.contains(galagoEnitityDoc2.name) && e2outGoingLinks.contains(entity.wikipediaTitle)

    if (linked) {
      addToAggregateMap(featurePrefix, isLinked, 1.0, aggregateMap)
    } else {
      addToAggregateMap(featurePrefix, isLinked, 0.0, aggregateMap)
    }

    if (mutualLinked) {
      addToAggregateMap(featurePrefix, mutualLink, 1.0, aggregateMap)
    } else {
      addToAggregateMap(featurePrefix, mutualLink, 0.0, aggregateMap)
    }



    val e1Combined = e1outGoingLinks ++ e1incomingLinks
    val e2Combined = e2outGoingLinks ++ e2incomingLinks
    val combinedLinkMeasures = SetMeasures(e1Combined, e2Combined)

    addToAggregateMap(featurePrefix, combinedLinkProxPMI, combinedLinkMeasures.approxPMI, aggregateMap)
    addToAggregateMap(featurePrefix, combinedLinkPMI, combinedLinkMeasures.PMI(numDocs), aggregateMap)
    addToAggregateMap(featurePrefix, combinedLinkIntersection, combinedLinkMeasures.intersect, aggregateMap)
    addToAggregateMap(featurePrefix, combinedLinkJaccard, combinedLinkMeasures.jaccard, aggregateMap)
    addToAggregateMap(featurePrefix, combinedLinkDice, combinedLinkMeasures.diceCorrect, aggregateMap)
    addToAggregateMap(featurePrefix, combinedLinkNormGoogleDistance, combinedLinkMeasures.normalizedGoogleDistance(numDocs), aggregateMap)

  }

  def categoryFeatures(galagoEnitityDoc1: Document, galagoEnitityDoc2: Document, aggregateMap: HashMap[String, ListBuffer[Double]]) {

    val e1Set = extractCategories(galagoEnitityDoc1)
    val e2Set = extractCategories(galagoEnitityDoc2)
   // println("num category feats: " + e1Set.size + " " + e2Set.size)
    val measures = SetMeasures(e1Set, e2Set)

    addToAggregateMap(featurePrefix, categoryIntersect, 1.0 * measures.intersect, aggregateMap)
    addToAggregateMap(featurePrefix, categoryMisses, 1.0 * measures.missNum, aggregateMap)
    addToAggregateMap(featurePrefix, categoryDiceOverlap, 1.0 * measures.diceCorrect, aggregateMap)
    addToAggregateMap(featurePrefix, categoryJaccardOverlap, 1.0 * measures.jaccard, aggregateMap)
    addToAggregateMap(featurePrefix, categoryCosine, 1.0 * measures.cosine, aggregateMap)


    // now break the categories down into words
    val lmSimilarity = LanguageModelFeatures.computeLmSimilarity(e1Set.mkString(" ").replace("category:", " "), e2Set.mkString(" ").replace("category:", " "), false)
    addToAggregateMap(featurePrefix, categoryVocabularyJaccard, lmSimilarity.jaccard, aggregateMap)
    addToAggregateMap(featurePrefix, categoryVocabularyJsDivergence, lmSimilarity.jsDivergence, aggregateMap)
    addToAggregateMap(featurePrefix, categoryVocabularyTfIdfCosine, lmSimilarity.cosine, aggregateMap)

    val e1TokenSet = TextNormalizer.normalizeText(e1Set.mkString(" ")).split("\\s+").filter(tok => !StopWordList.isStopWord(tok)).toSet
    val e2TokensSet = TextNormalizer.normalizeText(e2Set.mkString(" ")).split("\\s+").filter(tok => !StopWordList.isStopWord(tok)).toSet
    val tokenSetMeasures = SetMeasures(e1TokenSet, e2TokensSet)
    addToAggregateMap(featurePrefix, categoryVocabularySetCosine, tokenSetMeasures.cosine, aggregateMap)
  }


  def extractCategories(galagoEnitityDoc: Document): Set[String] = {

    val fieldsToCount = Set("category")

    val terms = galagoEnitityDoc.terms
    val fields = galagoEnitityDoc.tags
    val tokenMap = scala.collection.mutable.HashMap[String, ListBuffer[String]]()
    for (field <- fields) {
      if (fieldsToCount contains field.name) {
        tokenMap.getOrElseUpdate(field.name, ListBuffer[String]()) ++= terms.subList(field.begin, field.end)

      }
    }
    val categories = tokenMap.getOrElse("category", ListBuffer[String]())
    categories.toSet
  }


  def entity2entityLinkFeatures(mention: EntityMention, entity: WikipediaEntity) {

    // extract first pass entity neighbors with a confidence threshold.
    val links = mention.linkedMentions
    val linkThreshold = 0.5
    val linkedNeighbors = new ListBuffer[ScoredWikipediaEntity]
    for (mentions <- links) {
      val topLink = mentions.entityLinks.headOption
      topLink match {
        case Some(link) => {
          val topCand = topLink.get
          if (link.score > linkThreshold) {
           // println(topCand.wikipediaTitle + " " + topCand.score)
            linkedNeighbors += topCand
          }
        }
        case None => {}
      }
    }
    val limitedNeighbors = linkedNeighbors.sortBy(-_.score) take 100
    println("Linked neighbors:" + linkedNeighbors.size)

    val entityFullTextAllFields = entity.document.terms.mkString(" ")

    val scoreMap = mutable.HashMap[String, ListBuffer[Double]]()


    for (neighbor <- limitedNeighbors) {

      val neighborDoc = DocumentBridgeMap.getKbDocumentProvider.getDocument(neighbor.wikipediaTitle)
      val neighborText = neighborDoc.terms.mkString(" ")

      val equals = entity.wikipediaTitle equals neighbor.wikipediaTitle

      if (equals) {
        addToAggregateMap(featurePrefix, matchingEntity, 1.0, scoreMap)
        addToAggregateMap(featurePrefix, matchingEntityScore, neighbor.score, scoreMap)
      } else {
        addToAggregateMap(featurePrefix, matchingEntity, 0.0, scoreMap)
      }

      //textSimilarityFeatures(entityFullTextAllFields, neighborText, scoreMap)
      categoryFeatures(entity.document, neighborDoc, scoreMap)
      textContainmentFeatures(entity.wikipediaTitle, neighbor.wikipediaTitle, entityFullTextAllFields, neighborText, scoreMap)
      linkFeatures(entity, neighborDoc, scoreMap)

    }


    scoreMap.map(m => {
      val valueList = m._2.toList
      val min = valueList.min
      val sum = valueList.sum
      val max = valueList.max
      val average = valueList.sum / valueList.length.toDouble
      addValueFeature(featurePrefix, m._1 + "_min", min)
      addValueFeature(featurePrefix, m._1 + "_sum", sum)
      addValueFeature(featurePrefix, m._1 + "_max", max)
      addValueFeature(featurePrefix, m._1 + "_avg", average)

    })

  }


  def addToAggregateMap(featurePrefix:String, feature:String, value:Double, aggregateMap: HashMap[String, ListBuffer[Double]]) = {
    aggregateMap.getOrElseUpdate((featurePrefix+feature), ListBuffer[Double]()) += value
  }

}


object Entity2EntityFeaturesTest {
  def main(args: Array[String]) {

    val featureMap = scala.collection.mutable.Map[String, Double]()
    val prefixSeparator = " . "

    def addFeatureCall(prefix: String, category: String, value: String) {
      featureMap += (prefix + prefixSeparator + category + "=" + value -> 1.0)
    }

    def addFeatureValueCall(prefix: String, category: String, value: Double) {
      featureMap += (prefix + prefixSeparator + category -> value)
    }

    val neighborMention = new SimpleEntityMention(docId = "eng-WL-11-174611-12978627", entityType = "ORG", mentionId = "EL_00637", entityName = "AAA", fullText = "")
    val neighbors = Seq(new LinkedMention(neighborMention, Seq(new ScoredWikipediaEntity("American_Automobile_Association", -1, 10, 1))), new LinkedMention(neighborMention, Seq(new ScoredWikipediaEntity("Roadside_assistance", -1, 10, 1))))

    val mention = new SimpleEntityMention(docId = "eng-WL-11-174611-12978627", entityType = "ORG", mentionId = "EL_00637", entityName = "AAA", fullText = "", linkedMentions = neighbors)
    val entity = GalagoDoc2WikipediaEntity.idToEntity("American_Automobile_Association")

    val entityOnlyFeatures = new FeatureSetup(addFeatureCall, addFeatureValueCall) with Entity2EntityFeatures {}

    val document = DocumentBridgeMap.getKbDocumentProvider.getDocument(entity.wikipediaTitle)
    entity.document = document
    entity.incomingLinks = WikiLinkExtractor.simpleExtractorNoContext(document).map(a => a.destination).toSet
    entity.outgoingLinks = document.metadata.getOrElse("srcInlinks", "").split("\\s+").toSet
    entity.combinedLinks =  entity.incomingLinks ++ entity.outgoingLinks

    entityOnlyFeatures.entity2entityLinkFeatures(mention, entity)
    featureMap.keySet.toList.sortWith((s1, s2) => (s1 < s2)).map(p => println(p + "=" + featureMap.get(p).get))

  }
}

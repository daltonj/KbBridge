package edu.umass.ciir.kbbridge.features

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import edu.umass.ciir.kbbridge.data.{SimpleEntityMention, WikipediaEntity, EntityMention}
import edu.umass.ciir.kbbridge.nlp.{TextNormalizer, NaiveQueryContextBuilder}
import edu.umass.ciir.kbbridge.util.ConfInfo
import edu.umass.ciir.kbbridge.search.{RetrievalMap}
import edu.umass.ciir.kbbridge.text2kb.GalagoDoc2WikipediaEntity

trait NameVariantsFeatures extends QueryOnlyFeatureGenerator {

  override val featurePrefix = "nameVariants"

  def documentContextNameVariantFeatures(mention: EntityMention, entity: WikipediaEntity) {
    val contextAnalyzer = new NaiveQueryContextBuilder()

    var altNames = List[String]()
    try {
      val context = contextAnalyzer.buildContext(mention)
      altNames = context.altNames.toList
    } catch {
      case _ => println("Unable to get context for mention: " + mention)
    }

    val nameVariantMap = HashMap[String, ListBuffer[Double]]()

    val nameplusQuery = mention.entityName :: altNames
    for (altName <- nameplusQuery) {

      val wordFeatures = nameWordSimilarityFeatures(altName, entity.name)
      addToAggregateMap(wordFeatures.toMap, nameVariantMap)

      val charFeatures = characterSimilarityFeatures(altName, entity.name)
      addToAggregateMap(charFeatures.toMap, nameVariantMap)

      val fieldTokens = fieldTokenMap(entity.document)
      val fieldProbMap = fieldMatchFeatures(altName, fieldTokens)
      for ((field, (fieldProb, queryProb)) <- fieldProbMap) {
        nameVariantMap.getOrElseUpdate(fieldLikelihood + "_" + field, ListBuffer[Double]()) += fieldProb
        nameVariantMap.getOrElseUpdate(fieldProbability + "_" + field, ListBuffer[Double]()) += queryProb
      }

      val exactMatchFieldCounts = exactFieldMatchMap(altName, entity.document)
      val exactFieldFeatures = exactFieldMatchFeatures(exactMatchFieldCounts)
      addToAggregateMap(exactFieldFeatures.toMap, nameVariantMap)

      val linkProbs = linkProbability(TextNormalizer.normalizeText(altName), entity.document, exactMatchFieldCounts)
      addToAggregateMap(linkProbs.toMap, nameVariantMap)

    }

    nameVariantMap.map(m => {
      val valueList = m._2.toList
      val min = valueList.min
      val max = valueList.max
      val average = valueList.sum / valueList.length.toDouble
      addValueFeature(featurePrefix, m._1 + "_min", min)
      addValueFeature(featurePrefix, m._1 + "_max", max)
      addValueFeature(featurePrefix, m._1 + "_avg", average)

    })

  }

  def addToAggregateMap(features: Map[String, Double], aggregateMap: HashMap[String, ListBuffer[Double]]) = {
    features.map(e => (aggregateMap.getOrElseUpdate(e._1, ListBuffer[Double]()) += e._2))
  }


}

object NameVariantsTest {
  def main(args: Array[String]) {

    val featureMap = scala.collection.mutable.Map[String, Double]()
    val prefixSeparator = " . "

    def addFeatureCall(prefix: String, category: String, value: String) {
      featureMap += (prefix + prefixSeparator + category + "=" + value -> 1.0)
    }

    def addFeatureValueCall(prefix: String, category: String, value: Double) {
      featureMap += (prefix + prefixSeparator + category -> value)
    }

    val entity = GalagoDoc2WikipediaEntity.idToEntity("Food_and_Drug_Administration")

    val mention = new SimpleEntityMention(docId = "eng-NG-31-100906-10932919", entityType = "ORG", mentionId = "EL_00637", entityName = "fda", fullText = "")
    val queryOnlyFeatures = new FeatureSetup(addFeatureCall, addFeatureValueCall) with NameVariantsFeatures {}

    val searcher = RetrievalMap.getSearcher
    val document = searcher.getDocument(entity.wikipediaTitle)
    entity.document = document

    queryOnlyFeatures.documentContextNameVariantFeatures(mention, entity)
    featureMap.keySet.toList.sortWith((s1, s2) => (s1 < s2)).map(p => println(p + "=" + featureMap.get(p).get))

  }
}
package edu.umass.ciir.kbbridge.features

import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import org.lemurproject.galago.core.parse.Document
import scala.collection.mutable.ListBuffer
import edu.umass.ciir.models._
import edu.umass.ciir.kbbridge.search.{RetrievalMap}
import edu.umass.ciir.kbbridge.nlp.TextNormalizer
import util.{SetMeasures, StringSimilarity}
import com.aliasi.spell.{EditDistance, JaroWinklerDistance}
import edu.umass.ciir.kbbridge.data.{SimpleEntityMention, WikipediaEntity, EntityMention}
import edu.umass.ciir.kbbridge.util.ConfInfo
import edu.umass.ciir.kbbridge.text2kb.GalagoDoc2WikipediaEntity

trait QueryOnlyFeatureGenerator extends FeatureGenerator {

  val featurePrefix = "query"

  // character set similarity
  val characterDiceScoreName = "characterDiceScore"
  val characterDiceLeftAlignScoreName = "characterDiceLeftAlign"
  val characterDiceRightAlignScoreName = "characterDiceRightAlign"
  val characterDiceAlignedMaxName = "characterDiceAlignMax"
  val characterIntersectName = "characterIntersect"
  val characterMissNumName = "characterMissNum"
  val characterJaccardName = "characterJaccard"

  // word set similarity  
  val wordMatchName = "wordMatch"
  val wordMissNumName = "wordMisses"
  val wordDiceName = "wordDice"
  val wordJaccardName = "wordJaccard"
  val wordDiceLeftAlignScoreName = "wordDiceAlignLeftScore"
  val wordDiceRightAlignScoreName = "wordDiceAlignRightScore"
  val wordDiceAlignedMaxName = "wordDiceAlignMaximum"

  // acronym specific features
  val acrotestFirstLetterMatchName = "firstLetterMatch"
  val isAcronymName = "isAcronym"
  val acronymMatchName = "acronymMatchName"

  // general distance / similarity features   
  val exactMatchName = "exactNameMatch"
  val prefixMatchName = "prefixNameMatch"
  val suffixMatchName = "suffixNameMatch"
  val levensteinDistanceName = "levensteinEditDistance"
  val jaroWinklerDistanceName = "jaroWinklerDistance"

  // query field based matches on the KB entry
  // these are binary features that correspond to 
  // an exact match of the entity to a field
  val exactMatchCount = "exactMatchCount"
  val exactMatchBool = "exactMatchBool"
  val exactMatchProb = "exactMatchProb"
  val exactFieldNumUniqueMatch = "exactFieldNumUniqueMatch"
  val exactFieldNumTotalMatches = "exactFieldNumTotalMatches"
  val fieldLikelihood = "fieldLikelihood"
  val fieldProbability = "fieldProbability"

  // link count
  val inlinkCount = "inlinks"
  val stanfordWebInlinkCount = "stanfExternalinlinks"

  // link probability
  val linkProb = "linkProb"
  val externalLinkProb = "externalLinkProb"

  val searcher = RetrievalMap.getSearcher


  def exactFieldMatchMap(mentionName: String, galagoEnitityDoc: Document) = {
    val fields = galagoEnitityDoc.tags;
    val terms = galagoEnitityDoc.terms;

    val fieldsToCount = Set("stanf_anchor-exact", "anchor-exact", "title-exact", "redirect-exact", "fbname-exact")
    var fieldExactMatchSum = 0;
    val fieldMatchMap = new HashMap[String, Int]

    val normalizedQuery = TextNormalizer.normalizeText(mentionName)

    for (f <- fields) {
      if (fieldsToCount contains f.name) {
        val fieldValue = terms.subList(f.begin, f.end).mkString("")
        if (normalizedQuery.length() > 0 && (TextNormalizer.normalizeText(fieldValue) equals normalizedQuery)) {
          val curValue = fieldMatchMap.getOrElse(f.name, 0)
          fieldMatchMap += (f.name -> (curValue + 1))
        }
      }
    }
    fieldMatchMap
  }

  def exactFieldMatchFeatures(exactFieldMatchCounts: HashMap[String, Int]) = {
    var fieldExactMatchSum = 0
    val featureValues = HashMap[String, Double]()
    val fieldsToCount = Set("stanf_anchor-exact", "anchor-exact", "title-exact", "redirect-exact", "fbname-exact")
    for (field <- fieldsToCount) {
      val count = exactFieldMatchCounts.getOrElse(field, 0)
      featureValues += ((exactMatchCount + "_" + field) -> count)
      fieldExactMatchSum += count
      if (count > 0) {
        featureValues += (exactMatchBool + "_" + field -> 1.0)
      } else {
        featureValues += (exactMatchBool + "_" + field -> 0.0)
      }
    }

    featureValues += (exactFieldNumUniqueMatch -> exactFieldMatchCounts.keySet.size)
    featureValues += (exactFieldNumTotalMatches -> logTf(fieldExactMatchSum))
    featureValues
  }

  def logTf(frequency: Double): Double = {
    if (frequency < 0.01) {
      0.0
    } else {
      1 + math.log(frequency)
    }
  }


  def fieldTokenMap(galagoEnitityDoc: Document) = {
    val fieldsToCount = Set("stanf_anchor", "anchor", "title", "redirect", "fbname")
    val terms = galagoEnitityDoc.terms;
    val fields = galagoEnitityDoc.tags;
    val tokenMap = scala.collection.mutable.HashMap[String, ListBuffer[String]]()
    for (f <- fields) {
      if (fieldsToCount contains f.name) {
        val fieldTokens = terms.subList(f.begin, f.end)
        val fieldString = fieldTokens.mkString(" ")

        val curTokenList = tokenMap.getOrElseUpdate(f.name, ListBuffer[String]()) ++= fieldTokens
      }
    }
    tokenMap
  }

  def fieldMatchFeatures(mention: String, tokenMap: HashMap[String, ListBuffer[String]]) = {
    val normalizedQuery = TextNormalizer.normalizeText(mention)
    val queryTokens = normalizedQuery.split(" ")
    val fieldProbMap = new HashMap[String, Tuple2[Double, Double]]()
    for (field <- tokenMap.keySet) {

      val tokens = tokenMap.get(field).get.toSeq

      val fieldLm = new LanguageModel(5)
      fieldLm.addDocument(tokens, false)
      fieldLm.calculateProbabilities()

      var fieldProb = 0.0d
      for (q <- queryTokens) {
        // FIX with smoothing from background collection
        val te = fieldLm.getTermEntry(q)
        var prob = 0.0000001
        if (te != null) {
          prob = te.getProbability()
          if (prob < 0.0 || prob > 1.0) {
            println("bad probability!")
            prob = 0.0000001
          }
        }
        fieldProb += Math.log(prob)
      }

      val qte = fieldLm.getTermEntry(normalizedQuery)
      var queryProb = 0.0
      if (qte != null) {
        queryProb = qte.getProbability()
        if (queryProb < 0.0 || queryProb > 1.0) {
          println("bad probability!")
          queryProb = 0.0
        }
      }

      fieldProbMap.put(field, (fieldProb, queryProb))
    }

    fieldProbMap
  }

  def inlinkFeatures(galagoEnitityDoc: Document) = {
    val featureValues = HashMap[String, Double]()

    var inlink = galagoEnitityDoc.metadata.getOrElse("inlink", "0").toInt
    if (inlink <= 0) {
      inlink = 1;
    }
    featureValues += (inlinkCount -> math.log(inlink))

    var stanfordInlink = galagoEnitityDoc.metadata.getOrElse("externalLinkCount", "0").toInt
    if (stanfordInlink <= 0) {
      stanfordInlink = 1;
    }
    featureValues += (stanfordWebInlinkCount -> math.log(stanfordInlink))
  }

  def linkProbability(mentionName: String, galagoEnitityDoc: Document, exactCountMap: HashMap[String, Int]) = {

    val featureValues = HashMap[String, Double]()

    if (mentionName.length() > 0) {
      val totalAnchors = searcher.getFieldTermCount(mentionName, "anchor-exact") + 0.5
      val exactAnchorCount = exactCountMap.getOrElse("anchor-exact", 0)
      if (totalAnchors < exactAnchorCount) {
        //  println("error getting inlink count feature:" + totalAnchors + " " + exactAnchorCount + " " + mention.entityName)

        // approximate by using in-doc stats
        var inlink = galagoEnitityDoc.metadata.getOrElse("inlink", "0").toInt
        if (inlink <= 0) {
          inlink = 1;
        }
        featureValues += (linkProb -> exactAnchorCount.toDouble / inlink)

      } else {
        val linkProbVal = exactAnchorCount / totalAnchors.toDouble
        featureValues += (linkProb -> linkProbVal)
      }
    } else {
      featureValues += (linkProb -> 0.0)
    }

    if (mentionName.length() > 0) {
      val totalStanfAnchors = searcher.getFieldTermCount(mentionName, "stanf_anchor-exact") + 0.5
      val exactStanfAnchorCount = exactCountMap.getOrElse("stanf_anchor-exact", 0)
      if (totalStanfAnchors < exactStanfAnchorCount) {
        // println("error getting stanford inlink count feature:" + totalStanfAnchors + " " + exactStanfAnchorCount + " " + mention.entityName)
        var stanfordInlink = galagoEnitityDoc.metadata.getOrElse("externalLinkCount", "0").toInt
        if (stanfordInlink <= 0) {
          stanfordInlink = 1;
        }
        featureValues += (externalLinkProb -> exactStanfAnchorCount.toDouble / stanfordInlink)
      } else {
        val stanfLinkProb = exactStanfAnchorCount / totalStanfAnchors.toDouble
        featureValues += (externalLinkProb -> stanfLinkProb)
      }
    } else {
      featureValues += (externalLinkProb -> 0.0)
    }
    featureValues
  }

  def characterSimilarityFeatures(mentionString: String, entityString: String) = {
    val normalizedMentionName = TextNormalizer.normalizeText(mentionString)
    val normalizedEntityName = TextNormalizer.normalizeText(entityString)
    val featureValues = HashMap[String, Double]()
    // exact name match
    featureValues += (exactMatchName -> StringSimilarity.exactMatch(normalizedMentionName, normalizedEntityName))
    // entity starts with mention name
    featureValues += (prefixMatchName -> StringSimilarity.prefixOf(normalizedEntityName, normalizedMentionName))
    // entity ends with mention name
    featureValues += (prefixMatchName -> StringSimilarity.suffixOf(normalizedEntityName, normalizedMentionName))

    // character level approximate similarity on name
    val normalizedMentionNameChars = normalizedMentionName.toCharArray.toSet
    val normalizedEntityNameChars = normalizedEntityName.toCharArray.toSet
    val characterSetSimilarity = SetMeasures(normalizedMentionNameChars.toSet, normalizedEntityNameChars.toSet)
    featureValues += (characterIntersectName -> 1.0 * characterSetSimilarity.intersect)
    featureValues += (characterMissNumName -> 1.0 * characterSetSimilarity.missNum)
    featureValues += (characterDiceScoreName -> characterSetSimilarity.diceCorrect)
    featureValues += (characterJaccardName -> characterSetSimilarity.jaccard)

    // dice character alignment scores
    val maxLen = math.min(normalizedMentionName.length(), normalizedEntityName.length())
    val diceLeft = {
      val mentionCutLeft = normalizedMentionName.substring(0, maxLen)
      val entityCutLeft = normalizedEntityName.substring(0, maxLen)
      val dice = StringSimilarity.diceCharacter(mentionCutLeft, entityCutLeft)
      dice
    }
    featureValues += (characterDiceLeftAlignScoreName -> diceLeft)

    val diceRight = {
      val mentionCutRight = normalizedMentionName.substring(normalizedMentionName.length - maxLen)
      val entityCutRight = normalizedEntityName.substring(normalizedEntityName.length - maxLen)
      val dice = StringSimilarity.diceCharacter(mentionCutRight, entityCutRight)
      dice
    }
    featureValues += (characterDiceRightAlignScoreName -> diceRight)

    val diceCharacterAlignedMax = math.max(diceLeft, diceRight)
    featureValues += (characterDiceAlignedMaxName -> diceCharacterAlignedMax)

    // edit distance metrics from Lingpipe.
    val levenstein = EditDistance.TRANSPOSING.distance(normalizedMentionName, normalizedEntityName)
    featureValues += (levensteinDistanceName -> levenstein)

    val jarowinkler = JaroWinklerDistance.JARO_WINKLER_DISTANCE.distance(normalizedMentionName, normalizedEntityName)
    featureValues += (jaroWinklerDistanceName -> jarowinkler)
    featureValues

  }

  def nameWordSimilarityFeatures(mention: String, entity: String) = {
    val normalizedMentionName = TextNormalizer.normalizeText(mention)
    val normalizedEntityName = TextNormalizer.normalizeText(entity)
    val featureValues = HashMap[String, Double]()
    // word similarity features
    val mentionWords = normalizedMentionName.split("\\s+")
    val entityWords = normalizedEntityName.split("\\s+")
    val maxLen = math.min(mentionWords.size, entityWords.size)

    val wordSetSimilarity = SetMeasures(mentionWords.toSet, entityWords.toSet)
    featureValues += (wordMatchName -> 1.0 * wordSetSimilarity.intersect)
    featureValues += (wordMissNumName -> 1.0 * wordSetSimilarity.missNum)
    featureValues += (wordDiceName -> wordSetSimilarity.diceCorrect)
    featureValues += (wordJaccardName -> wordSetSimilarity.jaccard)

    // dice word aligned scores
    val maxWordLen = math.min(mentionWords.size, entityWords.size)
    val diceWordLeft = {
      val mtextSplit = mentionWords.slice(0, maxLen).toSet
      val etextSplit = entityWords.slice(0, maxLen).toSet
      val dice = SetMeasures(mtextSplit, etextSplit).diceCorrect
      dice
    }
    featureValues += (wordDiceLeftAlignScoreName -> diceWordLeft)

    val diceWordRight = {
      val mtextSplit = mentionWords.slice(mentionWords.size - maxLen, mentionWords.size).toSet
      val etextSplit = entityWords.slice(entityWords.size - maxLen, entityWords.size).toSet
      val dice = SetMeasures(mtextSplit, etextSplit).diceCorrect
      dice
    }
    featureValues += (wordDiceRightAlignScoreName -> diceWordRight)

    val diceWordMax = math.max(diceWordLeft, diceWordRight)
    featureValues += (wordDiceAlignedMaxName -> diceWordMax)
    featureValues
  }

  def acronymFeatures(mention: EntityMention, entity: WikipediaEntity) = {
    val mentionName = mention.entityName
    val entityName = entity.wikipediaTitle
    val featureValues = HashMap[String, Double]()
    if (mentionName.forall(c => c.isUpper)) {
      featureValues += (isAcronymName -> 1.0)
    } else {
      featureValues += (isAcronymName -> 0.0)
    }

    val firstCharMatch = StringSimilarity.firstCharacterMatch(mentionName, entityName)
    featureValues += (acrotestFirstLetterMatchName -> firstCharMatch)

    val possibleAcronym = StringSimilarity.acronymTest(mentionName, entityName)
    featureValues += (acronymMatchName -> possibleAcronym)
    featureValues
  }


  def generateQueryOnlyFeatures(mention: EntityMention, entity: WikipediaEntity, otherCands: Seq[WikipediaEntity]) {
    val acronymFeats = acronymFeatures(mention, entity)
    mapToFeature(acronymFeats.toMap)

    val wordFeatures = nameWordSimilarityFeatures(mention.entityName, entity.wikipediaTitle)
    mapToFeature(wordFeatures.toMap)

    val charFeatures = characterSimilarityFeatures(mention.entityName, entity.wikipediaTitle)
    mapToFeature(charFeatures.toMap)

    val fieldTokens = fieldTokenMap(entity.document)
    val fieldProbMap = fieldMatchFeatures(mention.entityName, fieldTokens)
    for ((field, (fieldProb, queryProb)) <- fieldProbMap) {
      addValueFeature(featurePrefix, fieldLikelihood + "_" + field, fieldProb)
      addValueFeature(featurePrefix, fieldProbability + "_" + field, queryProb)
    }

    val exactMatchFieldCounts = exactFieldMatchMap(mention.entityName, entity.document)

    val exactFieldFeatures = exactFieldMatchFeatures(exactMatchFieldCounts)
    mapToFeature(exactFieldFeatures.toMap)

    val linkPopularity = inlinkFeatures(entity.document)
    mapToFeature(linkPopularity.toMap)

    val linkProbs = linkProbability(TextNormalizer.normalizeText(mention.entityName), entity.document, exactMatchFieldCounts)
    mapToFeature(linkProbs.toMap)
  }

  def mapToFeature(features: Map[String, Double]) = {
    features.map(e => addValueFeature(featurePrefix, e._1, e._2))
  }


}

object QueryOnlyTest {
  def main(args: Array[String]) {

    val featureMap = scala.collection.mutable.Map[String, Double]()
    val prefixSeparator = " . "

    def addFeatureCall(prefix: String, category: String, value: String) {
      featureMap += (prefix + prefixSeparator + category + "=" + value -> 1.0)
    }

    def addFeatureValueCall(prefix: String, category: String, value: Double) {
      featureMap += (prefix + prefixSeparator + category -> value)
    }

    val entity = GalagoDoc2WikipediaEntity.idToEntity("American_Civil_Liberties_Union")

    val searcher = RetrievalMap.getSearcher
    val document = searcher.getDocument(entity.wikipediaTitle)
    entity.document = document

    val mention = new SimpleEntityMention(docId = "eng-NG-31-101177-10944212", entityType = "ORG", mentionId = "EL_00012", entityName = "ACLU", fullText="")
    val queryOnlyFeatures = new FeatureSetup(addFeatureCall, addFeatureValueCall) with QueryOnlyFeatureGenerator {}

    queryOnlyFeatures.generateQueryOnlyFeatures(mention, entity, Seq())
    featureMap.keySet.toList.sortWith((s1, s2) => (s1 < s2)).map(p => println(p + "=" + featureMap.get(p).get))

  }
}

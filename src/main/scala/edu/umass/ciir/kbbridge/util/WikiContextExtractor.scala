package edu.umass.ciir.kbbridge.util

import org.lemurproject.galago.core.parse.Document
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import edu.umass.ciir.kbbridge.nlp.TextNormalizer
import edu.umass.ciir.kbbridge.search.DocumentBridgeMap


object WikiContextExtractor {

  val fieldsToCount = Set("anchor-exact", "redirect-exact", "fbname-exact")
// val fieldsToCount = Set("stanf_anchor-exact", "anchor-exact", "redirect-exact", "fbname-exact")

  def getAlternativeNames(entityName: String, galagoEnitityDoc: Document): Seq[String] = {
    val fields = galagoEnitityDoc.tags
    val terms = galagoEnitityDoc.terms

//    val fieldsToCount = Set("stanf_anchor-exact", "anchor-exact", "redirect-exact", "fbname-exact")
    var fieldExactMatchCount = 0
    val tokenMap = scala.collection.mutable.HashMap[String, ListBuffer[String]]()


    val normalizedQuery = TextNormalizer.normalizeText(entityName)

    for (f <- fields) {
      if (fieldsToCount contains f.name) {
        val fieldValue = terms.subList(f.begin, f.end).mkString("").trim()
        val curTokenList = tokenMap.getOrElseUpdate(f.name, ListBuffer[String]()) += fieldValue
        tokenMap.update(f.name, curTokenList)
      }
    }

    val alternativeNames = tokenMap.getOrElse("anchor-exact", Seq()) ++ tokenMap.getOrElse("redirect-exact", Seq()) ++ tokenMap.getOrElse("fbname-exact", Seq())
    val filtered = alternativeNames.filter(alternateName => !(normalizedQuery contains TextNormalizer.normalizeText(alternateName)) && !(normalizedQuery equals TextNormalizer.normalizeText(alternateName)) && !(TextNormalizer.normalizeText(alternateName).length == 0))
    filtered.toSet.toSeq
  }



  def getAnchorNameCounts(entityName: String, anchorField: String, galagoEnitityDoc: Document): Map[String, Int] = {
    val fields = galagoEnitityDoc.tags
    val terms = galagoEnitityDoc.terms

    val tokenMap = ListBuffer[String]()
    val normalizedQuery = TextNormalizer.normalizeText(entityName)

    for (f <- fields.filter(_.name == anchorField)) {
      val fieldValue = terms.subList(f.begin, f.end).mkString(" ").trim()
      tokenMap += fieldValue
    }

    val alternativeNames = SeqTools.countMap(tokenMap)

    val filtered = alternativeNames.filterKeys(alternateName => {
      val normAlternateName = TextNormalizer.normalizeText(alternateName)
      !(normalizedQuery contains normAlternateName) &&
      ! (normalizedQuery equals normAlternateName) &&
      ! (normAlternateName.length == 0)
    })
    filtered
  }

  def getAnchorProbs(termCounts: Map[String, Int], anchorField: String, getFieldTermCount:(String, String) => Long): Map[String, Double] = {

    val anchorProbs = new HashMap[String, Double]()
    for ((alternateName, inlinkCount) <- termCounts){
      val totalAnchorCount = getFieldTermCount(alternateName, anchorField) + 0.5

      if (totalAnchorCount <= inlinkCount) {
        anchorProbs += (alternateName -> 1.0)
      } else {
        val stanfLinkProb = 1.0 * inlinkCount / totalAnchorCount
        anchorProbs += (alternateName -> stanfLinkProb)
      }
    }
    anchorProbs.toMap
  }


  def getWeightedAnchorNames(entityName:String, galagoEntityDoc:Document, getFieldTermCount:(String, String) => Long):Map[String, Map[String,Double]] = {
    val result = new ListBuffer[(String, Map[String,Double])]()
    for(anchorField <- fieldsToCount)  {
      val nameCounts = getAnchorNameCounts(entityName, anchorField, galagoEntityDoc)
      val nameWeights = getAnchorProbs(nameCounts, anchorField, getFieldTermCount)
      result += (anchorField -> nameWeights)
    }
    result.toMap
  }

}
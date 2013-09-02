package edu.umass.ciir.kbbridge.kb2text

import edu.umass.ciir.kbbridge.data.repr.EntityRepr
import edu.umass.ciir.kbbridge.util.{SeqTools, WikiContextExtractor}
import edu.umass.ciir.kbbridge.nlp.TextNormalizer
import collection.mutable.ListBuffer
import org.lemurproject.galago.core.parse.Document

/**
 * User: dietz
 * Date: 6/12/13
 * Time: 6:48 PM
 */
class StanfordAnchorEntityRepr(val neighborFeatureWeights:Map[String,Double], val buildM:Boolean = true, val getFieldTermCount:(String, String) => Long) {
  import WikiEntityRepr._

  val spamnames = Set("http","link","here","web","site","view","click","provided","source","original","download","website","live","article","read","wiki","quelle","background","artikel","encyclopedia","mehr","german","french","english","address")
  def filterSpamNames(name:String):Boolean = {
    spamnames.exists(name.contains(_))
  }

  def buildEntityRepr(wikipediaTitle:String, maskedGalagoDoc: Document, passageInfo:Seq[(Int,Int)]):EntityRepr = {


    val entityName = WikiEntityRepr.wikititleToEntityName(wikipediaTitle)
    val alternativeNameWeightsPerField = WikiContextExtractor.getWeightedAnchorNames(entityName, maskedGalagoDoc, getFieldTermCount, fieldsToCount = Set("stanf_anchor-exact"))




    // ============================
    // word context
    val stanf_anchor = alternativeNameWeightsPerField("stanf_anchor-exact")
    val topWeightedNamesRaw = SeqTools.topK(stanf_anchor.filterNot(entry => filterSpamNames(entry._1.toLowerCase)).toSeq, 100)

    val topWeightedNames = {
      val normalizeNames = topWeightedNamesRaw.map(entry => TextNormalizer.normalizeText(entry._1) -> entry._2) .filter(_._1.length>3)
      SeqTools.sumSeq(normalizeNames)
    }


    EntityRepr(entityName = entityName, queryId = Some(wikipediaTitle), nameVariants = topWeightedNames, neighbors = Seq.empty, words = Seq.empty)
  }




  def documentNeighborCount(wikipediaTitle:String, galagoDoc:Document):Seq[NeighborCount] = {


    val WikiNeighbors(outAnchors, inlinks, contextLinks) = findNeighbors(wikipediaTitle,galagoDoc)

    val passageLinks = outAnchors

    val destinations = passageLinks.groupBy(_.destination)

    val neighborWithCounts  =
    for ((destination, anchors) <- destinations) yield {
      val inlinkCount = if (inlinks.contains(destination)) {1} else {0}
      val contextCount = contextLinks(destination)
      val canonicalDestName = wikititleToEntityName(destination)
      NeighborCount(wikipediaTitle, destination, canonicalDestName, anchors, inlinkCount, contextCount)
    }

    neighborWithCounts.toSeq
  }


  def extractNeighbors(entityName:String, wikipediaTitle:String, maskedGalagoDoc:Document, passageInfo:Seq[(Int,Int)]): Seq[(EntityRepr, Double)] = {
    val usePassage = !passageInfo.isEmpty
    val passageText =
      if(!usePassage)  ""
      else maskedGalagoDoc.text

    val WikiNeighbors(links, inlinkCount, contextCount) = findNeighbors(wikipediaTitle,maskedGalagoDoc)
    val destinations = links.groupBy(_.destination)


    case class NeighborScores( paragraphScore:Double, outlinkCount:Int, hasInlink:Boolean, cooccurrenceCount:Int){
      def asFeatureVector:Seq[(String, Double)] =
        Seq(
          "paragraphScore" -> paragraphScore,
          "outlinkCount" -> outlinkCount.toDouble,
          "hasInlink" -> (if(hasInlink) 1.0 else 0.0),
          "cooccurrenceCount" -> cooccurrenceCount.toDouble
        )

      def asNormalizedFeatureVector(normalizer:Seq[(String,Double)]):Seq[(String,Double)] = {
        val normMap = normalizer.toMap
        for((key, value) <- asFeatureVector) yield key -> (value / normMap(key))
      }
    }


    def computeParagraphScore(pId:Int):Double = if(pId < 10) {1.0} else {0.1}
    val neighborinfo =
      (for ((destination, anchors) <- destinations) yield {
        val normDest = wikititleToEntityName(destination)

        val weightedParagraphNeighborSeq = new ListBuffer[(String, Double)]()
        for (anchor <- anchors)  {
          val paragraphScore = computeParagraphScore(anchor.paragraphId)
          val normalizedAnchorText = TextNormalizer.normalizeText(anchor.anchorText)

          if (usePassage){
            if(passageText contains anchor.rawAnchorText){
              weightedParagraphNeighborSeq += normalizedAnchorText -> paragraphScore
            }
          } else {
            weightedParagraphNeighborSeq += normalizedAnchorText -> paragraphScore
          }

        }
        val weightedParagraphNeighbors = SeqTools.groupByMappedKey[String, Double, String, Double](weightedParagraphNeighborSeq, by=TextNormalizer.normalizeText(_), aggr = _.sum)


        val neighborScores = {
          val paragraphScore = weightedParagraphNeighbors.map(_._2).sum
          val outlinkCount = anchors.length
          val hasInlink = inlinkCount.contains(destination)
          val cooccurrenceCount = contextCount(destination)
          NeighborScores(paragraphScore, outlinkCount, hasInlink, cooccurrenceCount)
        }
        ((destination,normDest), weightedParagraphNeighbors, neighborScores)
      }).toSeq

    val summed = SeqTools.sumDoubleMaps(neighborinfo.map(_._3.asFeatureVector.toMap))
    val weightedNeighbors: Seq[(EntityRepr, Double)] =
      for(((dest,normDest), names, neighborScores) <- neighborinfo) yield {
        val normalizedFeature = neighborScores.asNormalizedFeatureVector(summed.toSeq)
        val score = SeqTools.innerProduct(normalizedFeature, neighborFeatureWeights)
        (EntityRepr(entityName = normDest, nameVariants = names, wikipediaTitleInput = Some(dest)) -> score)
      }

//    val neighborInfo_ = neighborinfo.map(entry => entry._1 -> (entry._2, entry._3)).toMap
//    val weightedNeighbors_ = weightedNeighbors.toMap

    if (weightedNeighbors.exists(_._2.isNaN())){
      println("nans in weightedNeighbors "+weightedNeighbors)
      println("neighborinfo "+neighborinfo)
    }

    weightedNeighbors



  }


}






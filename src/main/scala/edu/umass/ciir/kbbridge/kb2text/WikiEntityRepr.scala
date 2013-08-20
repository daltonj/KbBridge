package edu.umass.ciir.kbbridge.kb2text

import edu.umass.ciir.kbbridge.data.repr.EntityRepr
import edu.umass.ciir.kbbridge.search.DocumentBridgeMap
import edu.umass.ciir.kbbridge.data.GalagoBridgeDocument
import edu.umass.ciir.kbbridge.util.{StringTools, SeqTools, WikiContextExtractor, WikiLinkExtractor}
import edu.umass.ciir.kbbridge.nlp.TextNormalizer
import collection.mutable.ListBuffer
import org.lemurproject.galago.core.parse.Document

/**
 * User: dietz
 * Date: 6/12/13
 * Time: 6:48 PM
 */
class WikiEntityRepr(val neighborFeatureWeights:Map[String,Double], val buildM:Boolean = true, val getFieldTermCount:(String, String) => Long) {
  def buildEntityRepr(wikipediaTitle:String, maskedGalagoDoc: Document, passageInfo:Seq[(Int,Int)]):EntityRepr = {


    val entityName = wikipediaTitle.replaceAllLiterally("_", " ")
    val alternativeNameWeightsPerField = WikiContextExtractor.getWeightedAnchorNames(entityName, maskedGalagoDoc, getFieldTermCount)

    // ============================
    // alternate names
    val redirect = alternativeNameWeightsPerField("redirect-exact")
    val fbName = alternativeNameWeightsPerField("fbname-exact")
    val anchor = alternativeNameWeightsPerField("anchor-exact")



    val weightedNames =
      SeqTools.sumDoubleMaps[String]( Seq(
        multiplyMapValue[String](redirect, 1.0),
        multiplyMapValue[String](fbName, 1.0),
        multiplyMapValue[String](anchor, 0.5)
      ))

    val topWeightedNames = Seq(entityName -> 1.0) ++ SeqTools.topK(weightedNames.toSeq, 10)

    if(topWeightedNames.map(_._2).exists(_.isNaN)){
      println("topWeightedNames contains nan "+topWeightedNames)
      println(redirect)
      println(fbName)
      println(anchor)

    }


    // ============================
    // neighbors


    val topWeightedNeighbors =
      if(buildM){
      val weightedNeighbors = extractNeighbors(entityName, wikipediaTitle, maskedGalagoDoc, passageInfo)
      SeqTools.topK(weightedNeighbors, 10)
    } else Seq.empty


    // ============================
    // word context
//    val stanf_anchor = alternativeNameWeightsPerField("stanf_anchor-exact")
//    val topWords = SeqTools.topK(stanf_anchor.toSeq, 10)
    val topWords = Seq()

    EntityRepr(entityName = entityName, queryId = Some(wikipediaTitle), nameVariants = topWeightedNames, neighbors = topWeightedNeighbors, words = topWords)
  }

  def ignoreWikiArticle(destination:String):Boolean = {
    val r = destination.startsWith("Category:") ||
      destination.startsWith("File:") ||
      destination.startsWith("List of ")
    r
  }

  def extractNeighbors(entityName:String, wikipediaTitle:String, maskedGalagoDoc:Document, passageInfo:Seq[(Int,Int)]): Seq[(EntityRepr, Double)] = {
    val links = WikiLinkExtractor.simpleExtractorNoContext(maskedGalagoDoc)
    val usePassage = !passageInfo.isEmpty
    val passageText =
      if(!usePassage)  ""
      else maskedGalagoDoc.text

    val destinations = links.groupBy(_.destination)
      .filterKeys(destination=>{
        (destination != wikipediaTitle) &&
          !ignoreWikiArticle(destination)
      })

    val inlinkCount = srcInLinks(maskedGalagoDoc)
    val contextCount = contextLinkCoocurrences(maskedGalagoDoc).toMap.withDefaultValue(0)


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

    def wikititleToEntityName(wikititle:String):String = {
      StringTools.zapParentheses(wikititle.replaceAllLiterally("_"," "))
    }

    def computeParagraphScore(pId:Int):Double = if(pId < 10) {1.0} else {0.1}
    val neighborinfo =
      (for ((destination, anchors) <- destinations) yield {
        val normDest = wikititleToEntityName(destination)

        val weightedParagraphNeighborSeq = new ListBuffer[(String, Double)]()
//        val weightedPassageNeighborSeq = new ListBuffer[(String, Double)]()
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
//        val weightedPassageNeighbors = SeqTools.groupByMappedKey[String, Double, String, Double](weightedPassageNeighborSeq, by=TextNormalizer.normalizeText(_), aggr = _.sum)


        val neighborScores = {
          val paragraphScore = weightedParagraphNeighbors.map(_._2).sum
          val outlinkCount = anchors.length
          val hasInlink = inlinkCount.contains(destination)
          val cooccurrenceCount = contextCount(destination)
          NeighborScores(paragraphScore, outlinkCount, hasInlink, cooccurrenceCount)
        }
        (normDest, weightedParagraphNeighbors, neighborScores)
      }).toSeq

    val summed = SeqTools.sumDoubleMaps(neighborinfo.map(_._3.asFeatureVector.toMap))
    val weightedNeighbors: Seq[(EntityRepr, Double)] =
      for((normDest, names, neighborScores) <- neighborinfo) yield {
        val normalizedFeature = neighborScores.asNormalizedFeatureVector(summed.toSeq)
        val score = SeqTools.innerProduct(normalizedFeature, neighborFeatureWeights)
        (EntityRepr(entityName = normDest, nameVariants = names) -> score)
      }

//    val neighborInfo_ = neighborinfo.map(entry => entry._1 -> (entry._2, entry._3)).toMap
//    val weightedNeighbors_ = weightedNeighbors.toMap

    if (weightedNeighbors.exists(_._2.isNaN())){
      println("nans in weightedNeighbors "+weightedNeighbors)
      println("neighborinfo "+neighborinfo)
    }

    weightedNeighbors



  }

  def srcInLinks(galagoDoc:Document):Seq[String] = {
    galagoDoc.metadata.get("srcInlinks").split(" ")
  }

  def contextLinkCoocurrences(galagoDoc:Document):Seq[(String, Int)] = {
    for(line <- galagoDoc.metadata.get("contextLinks").split("\n")) yield {
      val title = StringTools.getSplitChunk(line, 0).get
      val countOpt = StringTools.toIntOption(StringTools.getSplitChunk(line, 1).getOrElse("0"))
      (title -> countOpt.getOrElse(0))
    }
  }

  def multiplyMapValue[K](m:Map[K,Double], scalar:Double):Map[K,Double] = {
    for((key,value) <- m) yield key -> (scalar * value)
  }

}





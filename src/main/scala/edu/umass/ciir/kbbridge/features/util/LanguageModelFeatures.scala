package edu.umass.ciir.kbbridge.features.util

import edu.umass.ciir.models._
import edu.umass.ciir.kbbridge.nlp.TextNormalizer

object LanguageModelFeatures {

  val defaultSmoothingValue = 2000

  def computeIdf(df:Long):Double = {
    val documentCount = 3507696 //searcher.numDocumentsInCollection
    //println("LMFEATURE:" + documentCount)
  //  println("LMFEATURE:" +searcher.totalCollectionFrequency)
    Math.log((documentCount - df + 0.5) / (df + 0.5));
  }
  
  def fetchIdfBatch(terms:Seq[String]):Map[String,  Double] = {
      terms.map( term => {
        (term, computeIdf(TermCountsMap.termFrequencyCountsMap(term)._2 ))
      }).toMap
  }
  
  case class SimilarityFeature(klDivergence:Double, jsDivergence:Double, jaccard:Double, cosine:Double);
  
  def computeLmSimilarity(e1: String, e2:String, performSmoothingFromBg: Boolean) : SimilarityFeature = {
    
     val e1Tokens = TextNormalizer.normalizeText(e1).split("\\s+").filter(tok => !StopWordList.isStopWord(tok))
     val e2Tokens = TextNormalizer.normalizeText(e2).split("\\s+").filter(tok => !StopWordList.isStopWord(tok))
      
      val mentionLm = new LanguageModel(1)
      mentionLm.addDocument(e1Tokens, true)
      mentionLm.calculateProbabilities()
      
      val candidateLm = new LanguageModel(1)
      candidateLm.addDocument(e2Tokens, true)
      candidateLm.calculateProbabilities()
      
      val backgroundLm = new LanguageModel(1)
      
      val allWords = (e1Tokens ++ e2Tokens).toSet[String]
      allWords.toSeq.map({case term => {
        val frequency = try {TermCountsMap.termFrequencyCountsMap(term)._1 } catch { case ex => 1 }
        backgroundLm.addEntry(new TermEntry(term, frequency, 1))
      }})
      backgroundLm.addDocument(mentionLm)
      backgroundLm.setCollectionFrequency(6060970848L);
      
      backgroundLm.calculateProbabilities()     

      var modelScorer = new KLDivergenceSimilarity(backgroundLm, defaultSmoothingValue)
      var klDivergenceScore = modelScorer.calculateSimilarity(mentionLm, candidateLm, !performSmoothingFromBg).getSimilarity()
      if (klDivergenceScore > 100) {
        klDivergenceScore = 100
      }
      
      
      var jsScorer = new JS_KLDivergenceSimilarity(backgroundLm, defaultSmoothingValue)
      var jsDivergenceScore = jsScorer.calculateSimilarity(mentionLm, candidateLm, !performSmoothingFromBg).getSimilarity()
      
      if (jsDivergenceScore > 100) {
        jsDivergenceScore = 100
      }
      
      
      var jaccardScorer = new JaccardSimilarity
      val jaccardSim = jaccardScorer.calculateSimilarity(mentionLm, candidateLm, !performSmoothingFromBg).getSimilarity()
      
      // compute cosine sim
      val baseTermMap = TfidfUtil.baseBatchMap(e1Tokens, fetchIdfBatch)
      
      val tfidfMap = TfidfUtil.updateBaseMap(e2Tokens, baseTermMap)
      val cosine = TfidfUtil.computeCosine(tfidfMap)
      
      return SimilarityFeature(klDivergenceScore,jsDivergenceScore, jaccardSim, cosine)
  }
}
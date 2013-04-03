package edu.umass.ciir.kbbridge.features.util


/**
 * 
 */

object TfidfUtil {
  case class TfIDfEntry(term:String, mCount:Int, idf:Double,  eCount:Int){
    def addETerm:TfIDfEntry =  {
      TfIDfEntry(term, mCount, idf, eCount +1)
    }
    def tfidf:Double = {
      1.0 * idf * mCount * eCount
    }

    def innerProduct:Double = {
      1.0 * mCount * eCount
    }

    def normFirst:Double = {
      (idf * mCount) * (idf * mCount)
    }
    def normSecond:Double = {
      (idf * eCount) * (idf * eCount)
    }
  }

  type TfIDFMap = Map[String,  TfIDfEntry]

  def baseMap(contextTerms:Seq[String], fetchIdf:(String => Double)):TfIDFMap = {
    val mHash = contextTerms.groupBy(x => x).map(entry => entry._1 -> entry._2.length)
    val mIdfHash = mHash.map(entry => (entry._1 -> TfIDfEntry(entry._1, entry._2, fetchIdf(entry._1), 0)))
    mIdfHash
  }

  def baseBatchMap(contextTerms:Seq[String], fetchIdfBatch:(Seq[String] => Map[String, Double])):TfIDFMap = {
    val mHash = contextTerms.groupBy(x => x).map(entry => entry._1 -> entry._2.length)
    val idf = fetchIdfBatch(mHash.keys.toSeq)
    val zipped = mHash.zip(idf)
    val mIdfHash =
      for(((term:String,  tf:Int),(term2:String, df:Double)) <- zipped) yield {
         if(term != term2) throw new RuntimeException("terms do not match, zip went wrong "+term+" - "+term2)
        (term -> TfIDfEntry(term, tf, df, 0))
      }
//    val mIdfHash = mHash.map(entry => (entry._1 -> TfIDfEntry(entry._1, entry._2, fetchIdf(entry._1), 0)))
    mIdfHash
  }

  def updateBaseMap(contextTerms: scala.Seq[String], baseMap: TfIDFMap): TfIDFMap ={
    val map =
      contextTerms.foldLeft(baseMap)({
        case (hash: TfIDFMap, term: String) => {
          val res = hash.get(term)
          if (res == None) hash
          else {
            hash.updated(term, res.get.addETerm)
          }
        }
      })

    map.filter(_._2.eCount>0)
  }

  def computeNorm(tfidfMap:TfIDFMap):(Double,  Double) = {
    val (first, second) = tfidfMap.values.map(entry => (entry.normFirst, entry.normSecond)).unzip
    (first.sum, second.sum)
  }

  //==================================
  //== Measures ==
  //--
  //-

  def computeTfidf(tfidfMap:TfIDFMap):Double = {
    val score = tfidfMap.values.map(_.tfidf).sum
    score
  }



  def computeCosine(tfidfMap:TfIDFMap):Double = {
    val num = 1.0 * tfidfMap.values.map(_.innerProduct).sum
    val (firstLen, secondLen) = computeNorm(tfidfMap)
    val den = 1.0 * math.sqrt(firstLen) *  math.sqrt(secondLen)
    val score =
      if(den == 0.0) 0.0 else {
      num/den
    }
    score
  }



  def computeTanimoto(tfidfMap:TfIDFMap):Double = {
//    throw new RuntimeException("I dont think this code is correct.")
    val (firstLen, secondLen) = computeNorm(tfidfMap)
    val num =  tfidfMap.values.map(_.innerProduct).sum
    val den = firstLen + secondLen - num


    val score =
      if(num == 0.0) 0.0
      else 1.0*num/den
    score
  }

}
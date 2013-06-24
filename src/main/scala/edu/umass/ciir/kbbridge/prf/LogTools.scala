package edu.umass.ciir.kbbridge.prf

/**
 * User: dietz
 * Date: 1/30/13
 * Time: 6:31 PM
 */
object LogTools {

  def logExpSumNormalize[A](docs: Seq[A], getScore:(A)=> Double):Seq[(A,Double)] = {
    val spotScores = docs.map(sd => sd -> getScore(sd))
    logExpSumNormalizeBase(spotScores)
  }


  def logExpSumNormalizeBase[A](docs: Seq[(A, Double)]):Seq[(A,Double)] = {
    val logExpSumNormalizer = LogTools.logExpSumNormalizer(docs.map(_._2))
    for ((spot, score) <- docs) yield {
      spot -> LogTools.normLogProb(score, logExpSumNormalizer)
    }
  }




  /**
   * Compute the marginal in log space
   * @param logProbs probabilities in log space
   * @return log marginal
   */
  def logExpSumNormalizer(logProbs:Seq[Double]):Double = {
    val pi = logProbs.max
    val exp_log_pi =
      for(logpi <- logProbs) yield {
        math.exp(logpi - pi)
      }

    val result = pi + math.log(exp_log_pi.sum)

    //    if (result> 0){
    //      throw new Error("logExpSumNormalizer yields "+result+". pi="+pi+" +  exp("+exp_log_pi.sum+") \nexp_log_pi  = Seq("+exp_log_pi.mkString("", ", ", "...")+ ")  \nlogProbs=Seq("+logProbs.mkString("", ", ", "...")+")")
    //    }
    if (result.isNaN){
      throw new Error("logExpSumNormalizer yields Nan. pi="+pi+" +  exp("+exp_log_pi.sum+") exp_log_pi  = "+exp_log_pi.take(10).mkString("", ", ", "...")+ "  logProbs="+logProbs.take(10).mkString("", ", ", "..."))
    }
    if (result.isInfinite){
      throw new Error("logExpSumNormalizer yields Infin. pi="+pi+" +  exp("+exp_log_pi.sum+") exp_log_pi  = "+exp_log_pi.take(10).mkString("", ", ", "...")+ "  logProbs="+logProbs.take(10).mkString("", ", ", "..."))
    }
    result
  }


  /**
   * Resurrect the probability [0,1] from the unnormalized log probability and the log marginal
   * @see #logExpSumNormalizer
   * @param logProb
   * @param logExpNorm compute with #logExpSumNormalizer
   * @return
   */
  def normLogProb(logProb:Double, logExpNorm:Double):Double = {
    math.exp(normLogProbLog(logProb, logExpNorm))
  }


  /**
   * normalize log probability, but stay in log space
   * @param logProb
   * @param logExpNorm
   * @return
   */
  def normLogProbLog(logProb:Double, logExpNorm:Double):Double = {
    val result = logProb - logExpNorm
    if (result> 0){
      throw new Error("normLogProbLog yields "+result+". logProb - "+logExpNorm)
    }
    if (result.isNaN){
      throw new Error("normLogProbLog yields "+result+". logProb - "+logExpNorm)
    }
    if (result.isInfinite){
      throw new Error("normLogProb yields Infin "+logProb+" / "+logExpNorm)
    }

    result
  }


  /**
   * Compute normalizer and resurrect probabilities from logProbs
   * @param logProbs  unnormalized probabilities in log space
   * @return normalized probabilities
   */
  def normLogProbs(logProbs:Seq[Double]):Seq[Double]= {
    val norm = logExpSumNormalizer(logProbs)
    for (logPi <- logProbs) yield {
      normLogProb(logPi, norm)
    }
  }


  def logExpDiff(logProbMax:Double, logProbMin:Double):Option[Double] = {
    if (logProbMax == logProbMin) return None
    val pi = logProbMax
    val exp_log_pi_diff =
      math.exp(0) - math.exp(logProbMin - logProbMax)
    val result = pi + math.log(exp_log_pi_diff)

    if (result> 0){
      throw new Error("logExpSumNormalizer yields "+result+". pi="+pi+" +  exp("+exp_log_pi_diff+") logProbMax  = "+logProbMax+" logProbMin = "+logProbMin)
    }
    if (result.isNaN){
      throw new Error("logExpSumNormalizer yields Nan. pi="+pi+" +  exp("+exp_log_pi_diff+") logProbMax  = "+logProbMax+" logProbMin = "+logProbMin)
    }
    if (result.isInfinite){
      throw new Error("logExpSumNormalizer yields Infin. pi="+pi+" +  exp("+exp_log_pi_diff+") logProbMax  = "+logProbMax+" logProbMin = "+logProbMin)
    }
    Some(result)
  }


  def main(args:Array[String]) {
    val testseq = Seq(0.1, 0.02, 0.08, 0.45, 0.05, 0.3)

    val logtest = testseq.map(math.log(_))
    println("org: "+testseq)
    println("in:  "+logtest)
    println("out: "+normLogProbs(logtest))

  }

}

package edu.umass.ciir.kbbridge.util

/**
 * User: jdalton
 * Date: 4/1/13
 */
object PrintConfInfo {
  def printTacConfig() {
    if (ConfInfo.galagoUseLocalIndex) {
      println("galagoParameterFile "+ ConfInfo.galagoJsonParameterFile)
    }
    println("candidateQueryType "+ConfInfo.candidateQueryType)
    println("maxCandidates "+ConfInfo.maxCandidates)
    println("useOracleCandidateGeneration "+ConfInfo.useOracleCandidateGeneration)
    println("rankingFeatures "+ConfInfo.rankingFeatures.mkString(","))
    println("nilClassifyFeatures "+ConfInfo.nilClassifyFeatures.mkString(","))
    println("detailedEvalOutput "+ConfInfo.detailedEvalOutput)
    println("crossval mode (or competition mode)? " +ConfInfo.pipelineCrossVal)

    println("git hash "+ConfInfo.gitHash)
  }

}

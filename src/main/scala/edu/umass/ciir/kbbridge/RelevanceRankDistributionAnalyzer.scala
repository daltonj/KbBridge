package edu.umass.ciir.kbbridge

import features.FileFeatureLoader
import scala.collection.mutable.ListBuffer
import tac.TacQueryUtil
import scala.collection.JavaConversions._

object CandidateRelevanceDistributionAnalyzer extends App {


    // val trainingQueries = SimpleRankUtil.selectTrainingQueries
    // val testQueries = SimpleRankUtil.select2012TestQueries
    val allQueries = TacQueryUtil.allQueries() //.get("2012").get._2

    val relevantRankList = new ListBuffer[Int]()
    println("All queries: " + allQueries.size)
    var filteredQueries = allQueries.filterNot(_.isNilQuery)
    println("Non-Nil queries: " + filteredQueries.size)

    val mentionFeatures = FileFeatureLoader.loadProtobufDataForQueries(filteredQueries, args(0))

    var numFound = 0;
    for (mention <- mentionFeatures) {
      val candidates = mention.getMention.getCandidatesList
      val galagoRanking = candidates.sortBy(-_.getRank())

      var curRank = -1
      val candsWithRank = galagoRanking
      for (cand <- galagoRanking) {

        val found = (if (mention.getGroundTruthWikiTitle.equalsIgnoreCase(cand.getWikipediaTitle)) 1 else 0)

        if (found > 0) {
          println(mention.getMention.getMentionId + "\t" + (cand.getRank()) + "\t" + mention.getMention.getEntityName + "\t" + cand.getWikipediaTitle)
          relevantRankList += (cand.getRank() )
          curRank = (cand.getRank() )
          numFound += 1
        }
      }
      if (curRank == -1) {
        println(mention.getMention.getMentionId + "\t-1\t" + mention.getMention.getEntityName + "\t" + mention.getGroundTruthWikiTitle)
        relevantRankList += (-1)
      }

    }

    println("Candidate recall: " + numFound / filteredQueries.size.toDouble)
    val counts = relevantRankList.foldLeft(Map[Int, Int]()) {
      (m, c) => m.updated(c, m.getOrElse(c, 0) + 1)
    }
    for (i <- -1 to 100) {
      println(i + " " + counts.getOrElse(i,0))
    }

}
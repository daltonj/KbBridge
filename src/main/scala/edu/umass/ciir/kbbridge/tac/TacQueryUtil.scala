package edu.umass.ciir.kbbridge.tac

import edu.umass.ciir.kbbridge.util.ConfInfo
import collection.mutable
import edu.umass.ciir.kbbridge.data.{TacEntityMention, WikipediaEntity}

/**
 * User: jdalton
 * Date: 4/1/13
 */
object TacQueryUtil {

  val query2009TrainFile = ConfInfo.el2009QueryFile
  val anno2009TrainFile = ConfInfo.el2009AnnoFile

  val query2010TrainFile = ConfInfo.el2010trainQueryFile
  val anno2010TrainFile = ConfInfo.el2010trainAnnoFile

  val query2010EvalFile = ConfInfo.el2010evalQueryFile
  val anno2010EvalFile = ConfInfo.el2010evalAnnoFile

  val query2011EvalFile = ConfInfo.el2011QueryFile
  val anno2011EvalFile = ConfInfo.el2011AnnoFile

  val query2012EvalFile = ConfInfo.el2012QueryFile
  val anno2012EvalFile = ConfInfo.el2012AnnoFile

  def queriesByYear(): Map[String, (String, Seq[TacEntityMention])] = {
    val queriesByYear = new mutable.HashMap[String, (String, Seq[TacEntityMention])]()
    queriesByYear += ("2009" ->(ConfInfo.el2009AnnoFile, TacQueryLoader.loadQueries(query2009TrainFile, anno2009TrainFile)))
    queriesByYear += ("2010train" ->(ConfInfo.el2010trainAnnoFile, TacQueryLoader.loadQueries(query2010TrainFile, anno2010TrainFile)))
    queriesByYear += ("2010eval" ->(ConfInfo.el2010evalAnnoFile, TacQueryLoader.loadQueries(query2010EvalFile, anno2010EvalFile)))
    queriesByYear += ("2011" ->(ConfInfo.el2011AnnoFile, TacQueryLoader.loadQueries(query2011EvalFile, anno2011EvalFile)))
    queriesByYear += ("2012" ->(ConfInfo.el2012AnnoFile, TacQueryLoader.loadQueries(query2012EvalFile, anno2012EvalFile)))
    queriesByYear.toMap

  }

  def selectNonNilEvenOddSplitQueries(): (Seq[TacEntityMention], Seq[TacEntityMention]) = {

    var targetQueries = allQueries
    println("total queries:" + targetQueries.size)
    val nonNil = targetQueries.filterNot(q => q.isNilQuery)
    println("NonNil: " + nonNil.size)

    val nonNildistribution = nonNil.groupBy(t => t.entityType).map(entry => Pair(entry._1, entry._2.length))
    nonNildistribution.map(group => println(group._1 + "\t" + group._2 + "\t" + (group._2 / nonNil.size.toDouble)))

    val trainSplit = nonNil.filterNot(q => (q.mentionId.replace("EL", "").replace("_", "").replace("ENG", "").toInt % 2) == 0)
    val testSplit = nonNil.filterNot(q => (q.mentionId.replace("EL", "").replace("_", "").replace("ENG", "").toInt % 2) == 1)

    println("Training, odd: " + trainSplit.size)
    val distribution = trainSplit.groupBy(t => t.entityType).map(entry => Pair(entry._1, entry._2.length))
    distribution.map(group => println(group._1 + "\t" + group._2 + "\t" + group._2 / trainSplit.size.toDouble))

    println("Test, even: " + testSplit.size)
    val testSplitdistribution = testSplit.groupBy(t => t.entityType).map(entry => Pair(entry._1, entry._2.length))
    testSplitdistribution.map(group => println(group._1 + "\t" + group._2 + "\t" + group._2 / testSplit.size.toDouble))

    (trainSplit, testSplit)

  }


  def selectEvenOddSplitQueries(): (Seq[TacEntityMention], Seq[TacEntityMention]) = {

    var targetQueries = allQueries()
    println("total queries:" + targetQueries.size)

    val trainSplit = targetQueries.filterNot(q => (q.mentionId.replace("EL", "").replace("_", "").replace("ENG", "").toInt % 2) == 0)
    val testSplit = targetQueries.filterNot(q => (q.mentionId.replace("EL", "").replace("_", "").replace("ENG", "").toInt % 2) == 1)

    println("Training, odd: " + trainSplit.size)
    val distribution = trainSplit.groupBy(t => t.entityType).map(entry => Pair(entry._1, entry._2.length))
    distribution.map(group => println(group._1 + "\t" + group._2 + "\t" + group._2 / trainSplit.size.toDouble))
    println("total non-nil queries: " + trainSplit.filterNot(q => q.isNilQuery).size)

    println("Test, even: " + testSplit.size)
    val testSplitdistribution = testSplit.groupBy(t => t.entityType).map(entry => Pair(entry._1, entry._2.length))
    testSplitdistribution.map(group => println(group._1 + "\t" + group._2 + "\t" + group._2 / testSplit.size.toDouble))
    println("total non-nil queries: " + testSplit.filterNot(q => q.isNilQuery).size)

    (trainSplit, testSplit)

  }


  def allQueries(): Seq[TacEntityMention] = {
    TacQueryLoader.loadQueries(query2009TrainFile, anno2009TrainFile) ++
      TacQueryLoader.loadQueries(query2010TrainFile, anno2010TrainFile) ++
      TacQueryLoader.loadQueries(query2010EvalFile, anno2010EvalFile) ++
      TacQueryLoader.loadQueries(query2011EvalFile, anno2011EvalFile) ++
      TacQueryLoader.loadQueries(query2012EvalFile, anno2012EvalFile)
  }

  def candidateContainsTruth(mention: TacEntityMention, candidates: Seq[WikipediaEntity]): Boolean = {

    val candsContainTruth = candidates.find(cand => isTrueCandidate(mention, cand.wikipediaTitle))
    candsContainTruth match {
      case None => false
      case Some(trueCand) => true
    }
  }

  def isTrueCandidate(query: TacEntityMention, candidateWikiTitle: String): Boolean = {
    if (query.nodeId startsWith "NIL") {
      return false
    }
    val wikiTruth = query.groundTruthWikiTitle
    //println(wikiTruth + " " + candidate.wikipediaTitle)
    val result = wikiTruth match {
      case Some(tacWikiTitle) => {
        val tacTitle = tacWikiTitle
        val titleMatch = candidateWikiTitle equals tacTitle
        titleMatch
      }
      case None => {
        //System.err.println("Ground truth lookup for "+query.mentionId + " " + query.nodeId+ " " + query.name + " failed.")
        false
      }
    }
    //println("match? " + result);
    result
  }

}

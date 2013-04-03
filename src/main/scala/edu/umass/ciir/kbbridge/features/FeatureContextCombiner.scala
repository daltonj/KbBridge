package edu.umass.ciir.kbbridge.features

import java.io._
import scala.io.Source
import scala.collection.mutable.ListBuffer
import edu.umass.ciir.kbbridge.data.{IdMap, EntityMention}
import edu.umass.ciir.kbbridge.tac.TacQueryUtil


object ContextCombiner {

  val featureDir = "/usr/aubury/scratch2/jdalton/tac/score_components"

  val contextFeatures = List("raw", "nv", "all_prfentites_ner", "sent" /*,"all_uniform_ner", "sent", "all_prfentites_ner", "all_local_ner", "all_uniform_ner")*/)
  val featureDomainMap = contextFeatures.zipWithIndex.toMap

  def convertScoresToFeatures(outputFeatureFile: File, queries: Seq[TacEntityMention]) {
    val p = new java.io.PrintWriter(outputFeatureFile, "UTF-8")

    for (query <- queries) {
      val featureFile = new File(featureDir + File.separator + query.mentionId)
      println("Loading feature file: " + featureFile.getAbsolutePath())
      if (featureFile.exists) {
        val candidates = loadFeatures(featureFile)
        for (candidate <- candidates) {
          val sb = new StringBuilder
          val wikiTitle = IdMap.tacId2WikiTitleMap.get(query.nodeId) match {
            case Some(tacWikiTitle) => {
              tacWikiTitle
            }
            case None => {
              println("Ground truth lookup for " + query.mentionId + " " + " " + query.entityName + " failed.")
              ""
            }
          }
          if (candidate._1 equals wikiTitle) {
            sb append 1
          } else {
            sb append 0
          }
          sb append " "
          sb append "qid:"
          sb append query.mentionId
          sb append " "

          val features = candidate._2
          for (feature <- features) {
            val featureId = featureDomainMap.getOrElse(feature.contextSource, -1)
            if (featureId >= 0) {
              sb append (featureId + 1)
              sb append ":"
              sb append feature.score
              sb append " "
            }
          }
          sb append "#"
          sb append " "
          sb append candidate._1
          p.println(sb.toString)

        }
      } else {
        println("Unable to load feature file for query: " + query.mentionId)
      }
    }
    p.close()

  }

  def loadFeatures(runFile: File): Map[String, Seq[ContextFeature]] = {
    val records = new ListBuffer[ContextFeature]()
    try {
      val source = Source.fromFile(runFile, "UTF-8")
      for (line <- source.getLines) {
        val data = line.split("\\s+")
        records += new ContextFeature(data(0), data(1), data(2), data(3).toDouble)
      }
    } catch {
      case e: Exception => {
        println("Error loading file: " + runFile.getName())
      }
    }
    val candidateFeatures = records.toSeq.groupBy(r => r.result)
    candidateFeatures
  }

  case class ContextFeature(contextSource: String, query: String, result: String, score: Double)


  def main(args: Array[String]) {
    val trainingSvmFileName = new File("/usr/aubury/scratch2/jdalton/tac/lambda_train_sample_query_nv_prf_feats")
    val trainingQueries = TacQueryUtil.selectNonNilEvenOddSplitQueries()._1
    convertScoresToFeatures(trainingSvmFileName, trainingQueries)

    //    val testSvmFile = "./data/rankfeaturesTop50/test"
    //       (new File(testSvmFile).mkdirs())
    //    val testingInstances = loadTestInstances
    //    writeToSvmFile(testSvmFile, testingInstances)
  }

}
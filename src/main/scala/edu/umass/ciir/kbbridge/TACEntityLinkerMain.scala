package cc.refectorie.user.dietz.tacco.entitylinking.loadrun


import scala.collection.mutable.ListBuffer
import collection.mutable.HashMap
import java.io.File
import ciir.umass.edu.learning.RankerFactory

import java.io.PrintWriter
import edu.umass.ciir.kbbridge.tac.{TacPredictionWriter, TacQueryUtil}
import edu.umass.ciir.kbbridge.RankLibReranker
import edu.umass.ciir.kbbridge.eval.{GalagoEvaluator, TacLinkingEvaluator, TacMetricsCalculator}
import TacMetricsCalculator.LinkerQueryPrediction
import edu.umass.ciir.kbbridge.data.{ScoredWikipediaEntity, TacEntityMention, IdMap}
import edu.umass.ciir.kbbridge.nil.{NilClusterer, NilPredictorAndClassifierMain}
import edu.umass.ciir.kbbridge.features.FileFeatureLoader

object TACEntityLinkerMain {

  def main(args: Array[String]) {

    if (args.length == 0) {
      println("[train|test|all] ltr_model_dir base_eval_dir run_name")
    }

    val (trainSet, testSet) = {
      if (args(0) equals "train") {
        (true, false)
      } else if (args(0) equals "test") {
        (false, true)
      } else if (args(0) equals "all") {
        (true, true)
      } else {
        println("WARN: No valid train/test set selected.")
        (false, false)
      }
    }

    val ltrModels = List(
      ("lambdamart_map_local", "all_local", new RankerFactory().loadRanker("./ltr_new/lambdamart_map_allyears_odd_allfeats/")),
      ("lambdamart_map_local_score_components", "all", new RankerFactory().loadRanker("./ltr_new/lambdamart_map_allyears_odd_allfeats_score_components/")) //,
      /*    ("lambdamart_p1_local", "all_local", new RankerFactory().loadRanker("./ltr_new/lambdamart_p1_allyears_odd_allfeats/")),
          ("lambdamart_p1_local_score_components", "all", new RankerFactory().loadRanker("./ltr_new/lambdamart_p1_allyears_odd_allfeats_score_components/"))*/

      /* ("ca_map_query_only", "query_only", "./ltr_new/coordinate_ascent_all_odd_query_only"),
       ("ca_map_query_nv", "query_nv", "./ltr_new/coordinate_ascent_allyears_odd_query_nv"),
       ("ca_map_local", "all_local", "./ltr_new/coordinate_ascent_allyears_odd_allfeats/"),
       ("ca_map_local_score_components", "all", "./ltr_new/coordinate_ascent_allyears_odd_allfeats_score_components/")*/
    )
    //    val ltrModelFile = new File(args(1))
    //    if (!ltrModelFile.exists()) {
    //      throw new IllegalStateException("Supervised model file does not exist! " + ltrModelFile.getAbsolutePath())
    //    }
    //    val modelFile1 = "./ltr/ranklib7.model"
    //    val modelFile2 = "./ltr/ranklib6.model"
    //  //  val modelFile3 = "./ltr_new/coordinate_ascent_odd"
    //    val modelFile3 = "./ltr_new/coordinate_ascent_odd2012"
    //
    //    val modelFile4 = "./ltr/ranklib_new_combined1.model"
    //    val modelFile5 = "./ltr/ranklib_new_combined2.model"


    // base output directory + runId
    val outputEvalDir = "./" + args(2) + "/" + args(3)
    val outputDirFile = new File(outputEvalDir)
    (outputDirFile.mkdirs())

    // add tac specific output dir
    val tacOutputDir = outputEvalDir + "/tac"
    val rawDir = new File(tacOutputDir)
    (rawDir.mkdirs())




    val useTacOnly = args(4).toBoolean

    //  val featureSet = args(5)
    val retrievalFeatureWeights = List(
      ("query_only", List(("raw", 1.0)).toMap),
      ("query_nv", List(("raw", 0.42), ("nv", 0.58)).toMap),
      ("query_prf", List(("raw", 0.489), ("nv", 0.511)).toMap),
      ("query_local", List(("raw", 0.4), ("all_local_ner", 0.6)).toMap),
      ("query_nv_prf", List(("raw", 0.370), ("nv", 0.336), ("all_prfweight_ner", 0.294)).toMap),
      ("query_nv_local", List(("raw", 0.308), ("nv", 0.380), ("all_local_ner", 0.3126)).toMap),
      ("query_nv_sent", List(("raw", 0.332), ("nv", 0.396), ("sent", 0.273)).toMap),
      ("query_nv_uniform", List(("raw", 0.310), ("nv", 0.382), ("all_uniform_ner", 0.308)).toMap),
      ("query_nv_uniform_sent", List(("raw", 0.3366), ("nv", 0.405), ("all_uniform_ner", 0.2260), ("sent", -0.0325)).toMap),
      ("query_nv_local_sent", List(("raw", 0.30), ("nv", 0.37), ("all_local_ner", 0.264), ("sent", -0.0655)).toMap),
      ("query_nv_prf_sent", List(("raw", 0.321), ("nv", 0.293), ("all_prfweight_ner", 0.230), ("sent", 0.155)).toMap),

      ("all", List(("raw", 0.293), ("nv", 0.22736), ("all_prfweight_ner", 0.144), ("sent", 0.0732),
        ("all_prfentites_ner", 0.0733), ("all_local_ner", 0.0981), ("all_uniform_ner", 0.0914)).toMap)

    )

    val filteredWeights = retrievalFeatureWeights.filter(fw => fw._1 equals "query_nv_prf")


    if (trainSet) {
      val trainingQueries = TacQueryUtil.selectEvenOddSplitQueries._1
      val trainingInstances = FileFeatureLoader.loadProtobufDataForQueries(trainingQueries)
      //     val rerankedResults = RankLibReranker.rerank(trainingInstances, useTacOnly, ltrModelFile, retrievalFeatureWeights, featureSet)
      //     writeTACResults(trainingQueries, rerankedResults, "./eval/tac_train1", outputEvalDir)
    }

    if (testSet) {


      val querySet = TacQueryUtil.selectEvenOddSplitQueries()._2.toSet
      val queriesByYear = TacQueryUtil.queriesByYear
      println("query stats:")
      println("total queries: " + querySet.size)
      println("total non-nil queries: " + querySet.filterNot(q => q.isNilQuery).size)

      val summaryWriter = new PrintWriter(outputEvalDir + "/summary")
      for ((year, (annoFile, queries)) <- queriesByYear) {
        TacLinkingEvaluator.clearJudgments()
        TacLinkingEvaluator.loadGoldStandardFile(annoFile)
        println("Results for : " + year)
        var testQueries = queries.filter(q => querySet contains q)
        println("queries: " + testQueries.size)
        println("test non-nil queries: " + testQueries.filterNot(q => q.isNilQuery).size)

        val distribution = testQueries.groupBy(t => t.entityType).map(entry => Pair(entry._1, entry._2.length))
        distribution.map(group => println(group._1 + "\t" + group._2 + "\t" + (group._2 / testQueries.size.toDouble)))

        val tacOutputYearDir = new File(tacOutputDir + "/" + year)
        val resultsSummary = HashMap[String, Map[String, ListBuffer[Double]]]()
        tacOutputYearDir.mkdir()

        val testInstances = FileFeatureLoader.loadProtobufDataForQueries(testQueries)
        ltrModels.map(model => {

          val featureSet = {
            if (model._2 equals "all") {
              List("raw", "nv", "sent", "all_prfweight_ner", "all_prfentites_ner", "all_local_ner", "all_uniform_ner")
            } else {
              List()
            }
          }
          val reranker = new RankLibReranker("./ltr_new/lambdamart_map_allyears_odd_allfeats")
          val supervisedResults = reranker.rerankMentionBatchWithFeatures(testInstances)
          writeTACResults(testQueries, supervisedResults, tacOutputDir + "/" + year + "/" + model._1, outputEvalDir + "/" + year + "_" + model._1)
          val ranklibResults = GalagoEvaluator.evaluate(annoFile.replace(".tab", ".qrel"), supervisedResults, outputEvalDir + "/" + year + "_" + model._1)
          resultsSummary += (model._1 -> ranklibResults._2)
          summaryWriter.println(year + "," + model._1 + "," + ranklibResults._1.map(x => x._2).mkString(","))

        })


        //      retrievalFeatureWeights.map(retrievalFeatureWeights => {
        //      val baselineResults = GalagoOnlyRanker.evaluateRetrievalOnly(testQueries, useTacOnly, retrievalFeatureWeights._2)
        //      println("Retrieval baseline evaluation:")
        //      writeTACResults(testQueries, baselineResults, tacOutputDir + "/" + year + "/retrieval", outputEvalDir + "/" + retrievalFeatureWeights._1 + "_" + year +"_retrieval")
        //      val evaluationResults = GalagoEvaluator.evaluate(annoFile.replace(".tab",".qrel"), baselineResults, outputEvalDir + "/" + retrievalFeatureWeights._1 + "_" + year +"_retrieval")
        //      resultsSummary += (retrievalFeatureWeights._1 -> evaluationResults._2)
        //      summaryWriter.println(year + "," + retrievalFeatureWeights._1 + "," + evaluationResults._1.map(x => x._2).mkString(","))
        //      }
        //      )
        //      analyzeSignificance(resultsSummary, year, outputEvalDir)

        println("end year")
      }
      summaryWriter.close()
    }

  }

  //  def analyzeSignificance(methodResultMap: HashMap[String, Map[String, ListBuffer[Double]]], year: String, outputFilePrefix: String) {
  //    val significanceWriter = new PrintWriter(outputFilePrefix + "/" + year + ".significance")
  //
  //    val combinations = methodResultMap.keys.toList.combinations(2)
  //    for (c <- combinations) {
  //
  //      val first = c(0)
  //      val second = c(1)
  //
  //      // now we have results for all metrics
  //      val firstMetricMap = methodResultMap(first)
  //      val secondMetricMap = methodResultMap(second)
  //
  //      for (metric <- firstMetricMap.keys) {
  //        val firstResults = firstMetricMap(metric).toArray
  //        val secondResults = secondMetricMap(metric) toArray
  //        val ttester = new PairedTTest(first + "_" + second)
  //        val significance = ttester.evaluate(firstResults, secondResults)
  //        significanceWriter.println(first + "," + second + "," + metric + ", " + significance)
  //      }
  //
  //    }
  //    significanceWriter.close()
  //
  //  }

  def writeTACResults(queries: Seq[TacEntityMention], rerankedResults: Map[String, Seq[ScoredWikipediaEntity]], tacOutputDirPrefix: String, outputFilePrefix: String) {
    val finalResults = new ListBuffer[LinkerQueryPrediction]

    val rankingWriter = new PrintWriter(outputFilePrefix + ".ranking")

    val relevantRanks = new ListBuffer[Int]()

    for (q <- queries) {
      val results = rerankedResults.get(q.mentionId)



      results match {
        // no results, no wiki results
        case None => finalResults += new LinkerQueryPrediction(q, None, None)
        case Some(results) => {
          if (results.length == 0) {
            println("Results with no top result!?! " + q.mentionId)
            finalResults += new LinkerQueryPrediction(q, None, None)
          } else {

            val trueResult = q.groundTruthWikiTitle
            val foundIdx = trueResult match {
              case Some(tacWikiTitle) => {
                val idx = results.indexWhere(r => r.wikipediaTitle equals tacWikiTitle)
                (idx)
              }
              case None => {
                -1
              }
            }
            relevantRanks += foundIdx
            //            if (!TacQueryUtil.isNilQuery(q) && foundIdx < 0) {
            //              println("Missing in candidate set: " + q)
            //            }


            results.map(r => rankingWriter.println("%s Q0 %s %d %s KL".format(q.mentionId, r.wikipediaTitle, r.rank, "%10.8f".format(r.score))))
            val topResult = results.head
            val tacIdListOpt = IdMap.wikiTitle2TacIdMap.get(topResult.wikipediaTitle)
            tacIdListOpt match {
              case Some(tacIdList) if (tacIdList != "") => {
                // TAC result, return tac ID + wiki title
                finalResults += new LinkerQueryPrediction(q, Some(tacIdList.split(",")), Some(topResult.wikipediaTitle), topResult.score)
              }
              case _ => {
                // Result is returned, but it is a non-tac result
                finalResults += new LinkerQueryPrediction(q, None, Some(topResult.wikipediaTitle), topResult.score)
              }
            }
          }

        }
      }

    }
    writeRelevantDistribution(outputFilePrefix, relevantRanks)
    rankingWriter.close()
    println("\nWriting and Evaluating model: " + outputFilePrefix)
    writeRawRankOutput(outputFilePrefix, finalResults)
    println("Raw rank-only accuracy:")
    TacMetricsCalculator.evaluateMicroAverageAccuracy(finalResults, outputFilePrefix)

    //   TacPredictionWriter.writePredicted(finalResults, tacOutputDirPrefix + ".wikiprediction" )

    val wikiNilPredictions = NilPredictorAndClassifierMain.wikiOnly(finalResults)


    val thresholds = Array(-1000000, -1000, -1, -0.5, 0, 0.5)
    for (threshold <- thresholds) {
      val nilPredictions = NilPredictorAndClassifierMain.predictNils(finalResults, threshold)
      val clustered = NilClusterer.clusterNils(nilPredictions)
      TacPredictionWriter.writePredicted(clustered, tacOutputDirPrefix + "." + -threshold)
      TacMetricsCalculator.evaluateMicroAverageAccuracy(nilPredictions, outputFilePrefix + "_" + threshold)

    }


  }

  def writeRelevantDistribution(filePrefix: String, relevantRanks: ListBuffer[Int]) {
    val writer = new PrintWriter(filePrefix + ".answer_distribution")
    val counts = relevantRanks.foldLeft(Map[Int, Int]()) {
      (m, c) => m.updated(c, m.getOrElse(c, 0) + 1)
    }
    //val sortedCounts = counts.keySet.toList.sort( (e1, e2) => (e1 < e2))
    for (i <- -1 to 99) {
      writer.println(i + " " + counts.getOrElse(i, 0))
    }
    writer.close
  }

  def writeRawRankOutput(filePrefix: String, predictions: Seq[LinkerQueryPrediction]) {
    val writer = new PrintWriter(filePrefix + ".predictions")
    for (p <- predictions) {
      val string = p.query.mentionId + "\t" + p.query.entityName + "\t" + p.query.entityType + "\t" + p.tacId.getOrElse("NONE") + "\t" + p.wikiTitle.getOrElse("NONE").trim() + "\t" + p.score
      //println(p + " " + string)
      writer.println(string)
    }
    writer.close()
  }

}
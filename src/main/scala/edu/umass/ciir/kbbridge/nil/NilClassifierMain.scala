//package edu.umass.ciir.kbbridge.nil
//
///**
// * User: jdalton
// * Date: 4/3/13
// */
//object NilClassifierMain {
//
//
//  val logitNilClassifier = new LogisticRegressionNilClassifier()
//  val translator = new FeatureVectorTranslator()
//
//  def train() {
//  translator.normalizeVector(nilPredict.ClassifyFeatureDimensionDomain)
//  logitNilClassifier.train(translator.normalized, translator.labels)
//  val result = logitNilClassifier.predict(translator.translateVector(nilPredict.ClassifyFeatureDimensionDomain, featureVector.asInstanceOf[FeatureVectorVariable[String]]),queryId)
//
//  }
//}

package edu.umass.ciir.kbbridge.nil

import scala.collection.mutable.ListBuffer
import java.io.File
import edu.umass.ciir.kbbridge.eval.TacMetricsCalculator.LinkerQueryPrediction

object NilPredictorAndClassifierMain {


  //  def loadRankPredictions(predictionFile : String): Seq[LinkerQueryPrediction]  = {
  //
  //    val finalResults = new ListBuffer[LinkerQueryPrediction]
  //
  //     val f = io.Source.fromFile(predictionFile)
  //     for (line <- f.getLines()) {
  //
  //       try {
  //         val sp = line.split("\t")
  //
  //         val qid = sp(0)
  //         val name = sp(1)
  //         val nerType = sp(2)
  //         val tacIdString = sp(3)
  //         val tacId = if (tacIdString.length == 0) None else Some(tacIdString)
  //         val wikipediaTitleString = sp(4)
  //         val score = sp(5)
  //         val wikipediaTitle = if (wikipediaTitleString.length == 0) None else Some(wikipediaTitleString)
  //
  //         finalResults += new LinkerQueryPrediction(qid, name, nerType, tacId, wikipediaTitle, score.toDouble)
  //       } catch {
  //         // corrupt file; return none.
  //         case e: Exception => println("Bad line! " + line)
  //         throw e
  //       }
  //     }
  //    finalResults
  //  }


  def main(args: Array[String]) {
    val outputDirectory = "./eval2012crazyprf"
    val outputDirectoryFile = new File(outputDirectory)
    if (!outputDirectoryFile.exists) {
      outputDirectoryFile.mkdirs()
    }
    val predictionDirName = "./rankOutputFullWiki2012crazyprf"
    val predictionFile = new File(predictionDirName)

    for (file <- predictionFile.listFiles()) {
      println("**** Now loading prediction file: " + file.toString)
      // val baselinePredictions = loadRankPredictions(file.getAbsolutePath())
      val thresholds = Array(-1000, -0.13, -0.12, -0.11, -0.1, -0.09, -0.08, -0.07, -0.06, -0.05, -0.04)
      for (threshold <- thresholds) {
        println(threshold)
        //   val predictedNils = predictNils(baselinePredictions, threshold)
        //TacMetricsCalculator.evaluateMicroAverageAccuracy(predictedNils)
        //     val clustered = NilClusterer.clusterNils(predictedNils)
        //    val outputFile = outputDirectory + File.separator + file.getName() + -threshold
        //    println(outputFile)
        //  TacPredictionWriter.writePredicted(clustered,outputFile)
      }

    }
  }

  def wikiOnly(predictions: Seq[LinkerQueryPrediction], scoreThreshold: Double = Double.MinValue): Seq[LinkerQueryPrediction] = {

    val nilPredictions = new ListBuffer[LinkerQueryPrediction]
    for (p <- predictions) {
      var tacId = p.tacId.getOrElse(List("NIL"))
      if (tacId(0) equals "NIL") {
        nilPredictions += new LinkerQueryPrediction(p.query, None, p.wikiTitle, p.score)
      } else {
        nilPredictions += new LinkerQueryPrediction(p.query, Some(tacId), p.wikiTitle, p.score)
      }
    }
    nilPredictions
  }

  def predictNils(predictions: Seq[LinkerQueryPrediction], scoreThreshold: Double = Double.MinValue): Seq[LinkerQueryPrediction] = {

    val nilPredictions = new ListBuffer[LinkerQueryPrediction]

    for (p <- predictions) {
      var tacId = p.tacId.getOrElse(List("NIL"))
      if (p.score < scoreThreshold) {
        tacId = List("NIL")
      }

      if (tacId(0) equals "NIL") {
        if (p.score < scoreThreshold) {
          nilPredictions += new LinkerQueryPrediction(p.query, None, None, p.score)
        } else {
        nilPredictions += new LinkerQueryPrediction(p.query, None, p.wikiTitle, p.score)
        }
      } else {
        nilPredictions += new LinkerQueryPrediction(p.query, Some(tacId), p.wikiTitle, p.score)
      }

    }
    nilPredictions
  }

}

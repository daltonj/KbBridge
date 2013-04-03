//package edu.umass.ciir.kbbridge.nil
//
//import scala.collection.JavaConversions._
//import scala.collection.mutable.ListBuffer
//
///**
// * This class translates from factorie features to Lingpipe features.
// *
// * It performs features normalization and standardization.
// */
//class FeatureVectorTranslator() {
//
//  val qidMap = scala.collection.mutable.Map[String, Int]()
//  val labels = new ListBuffer[java.lang.Integer]
//  val vectors = new java.util.ArrayList[java.util.List[java.lang.Double]]
//  val normalized = new java.util.ArrayList[java.util.List[java.lang.Double]]
//  var meanSum = new Array[Double](0)
//  var stdDeviationSum = new Array[Double](0)
//  var isContinuous = Array.fill[Boolean](0)(false)
//
//  def translate(domain: CategoricalDomain[String], featureVector: FeatureVectorVariable[String], isNil: Boolean) {
//
//    if (isNil) {
//      labels += 1
//    } else {
//      labels += 0
//    }
//    println("Nil? : " + isNil);
//
//    println("domain size: " + domain.size)
//    val featureRange = featureVector.vector.activeDomain.toList
//    var features = Array.fill[java.lang.Double](domain.size + 1)(0.0d)
//
//    // intercept
//    features(0) = 1.0
//    featureRange.sorted.map(featureIndex => {
//      //val featureNm = domain.dimensionDomain.getCategory(featureIndex+1).toString
//      val featureValue = featureVector.vector(featureIndex)
//      features(featureIndex + 1) = featureValue
//    })
//
//    //      for ( (c, idx) <- domain.dimensionDomain.categoryValues.zipWithIndex) {
//    //        println(c + ": " + features(idx+1))
//    //      }
//
//    //println(features.size + ": " + features.mkString(" "))
//    vectors += features.toList
//
//  }
//
//  def normalizeVector(domain: CategoricalDomain[String]) {
//
//    isContinuous = Array.fill[Boolean](domain.size)(false)
//    meanSum = new Array[Double](domain.size)
//    stdDeviationSum = new Array[Double](domain.size)
//
//    // compute mean
//    for (vec <- vectors) {
//      for ((c, idx) <- domain.dimensionDomain.categoryValues.zipWithIndex) {
//        val featureValue = vec(idx + 1)
//        if (!(featureValue == 0 || featureValue == 1)) {
//          isContinuous(idx) = true
//        }
//        meanSum(idx) += featureValue
//      }
//    }
//
//    // compute mean
//    for ((c, idx) <- domain.dimensionDomain.categoryValues.zipWithIndex) {
//      meanSum(idx) = meanSum(idx) / vectors.size()
//    }
//
//    // compute std deviation
//    for (vec <- vectors) {
//      for ((c, idx) <- domain.dimensionDomain.categoryValues.zipWithIndex) {
//        val featureValue = vec(idx + 1)
//        val mean = meanSum(idx)
//        val deviation = (featureValue - mean) * (featureValue - mean)
//        stdDeviationSum(idx) += deviation
//      }
//    }
//
//    // std deviation for each feature
//    for ((c, idx) <- domain.dimensionDomain.categoryValues.zipWithIndex) {
//      stdDeviationSum(idx) = Math.sqrt(stdDeviationSum(idx) / vectors.size())
//    }
//
//
//    // normalize feature values
//    for (vec <- vectors) {
//
//      var normalizedFeatures = Array.fill[java.lang.Double](domain.size + 1)(0.0d)
//      // intercept
//      normalizedFeatures(0) = 1.0
//
//      for ((c, idx) <- domain.dimensionDomain.categoryValues.zipWithIndex) {
//        val featureValue = vec(idx + 1)
//
//        if (isContinuous(idx)) {
//          val normalized = (featureValue - meanSum(idx)) / stdDeviationSum(idx)
//          println(c + " raw: " + featureValue + " normalized: " + normalized)
//          normalizedFeatures(idx + 1) = normalized
//        } else {
//          normalizedFeatures(idx + 1) = featureValue
//        }
//      }
//      normalized += normalizedFeatures.toList
//    }
//
//  }
//
//  def translateVector(domain: CategoricalDomain[String], featureVector: FeatureVectorVariable[String]): java.util.List[java.lang.Double] = {
//    val featureRange = featureVector.vector.activeDomain.toList
//    val features = Array.fill[java.lang.Double](domain.size + 1)(0.0d)
//    features(0) = 1.0 // intercept
//
//    featureRange.sorted.map(featureIndex => {
//      //val featureName = domain.dimensionDomain.getCategory(featureIndex).toString
//      val featureValue = featureVector.vector(featureIndex)
//
//      if (isContinuous(featureIndex) && ConfInfo.normalizeFeatures) {
//        features(featureIndex + 1) = (featureValue - meanSum(featureIndex)) / stdDeviationSum(featureIndex)
//      } else {
//        features(featureIndex + 1) = featureValue
//      }
//
//
//    })
//
//    //       for ( (c, idx) <- domain.dimensionDomain.categoryValues.zipWithIndex) {
//    //        println(c + ": " + features(idx+1))
//    //      }
//    //      println(features.size + ": " + features.mkString(" ") + "\n")
//    features.toList
//  }
//}

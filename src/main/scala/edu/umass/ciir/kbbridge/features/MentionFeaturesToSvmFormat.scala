package edu.umass.ciir.kbbridge.features

import edu.umass.ciir.kbbridge.data.{ScoredWikipediaEntity, EntityMention}
import collection.{immutable, mutable}

/**
 * User: jdalton
 * Date: 4/3/13
 */
object EntityFeaturesToSvmConverter {


  def loadDomainFromFile() {
    val domainMapFile = "./data/ltr/domainMap"

    val domainMap = {
      val domainMap = new mutable.HashMap[String, Int]()
      val f = io.Source.fromFile(domainMapFile)
      for (line <- f.getLines()) {
        val fields = line.split("\t")
        domainMap += (fields(0) -> fields(1).toInt)
      }
      domainMap
    }
    val m2eDomainSet = domainMap.map(_._1).toSet
    val map = m2eDomainSet.zipWithIndex.toMap
    featureDomainMap = map
    println("Feature domain map loaded with: " + featureDomainMap.size + " entries")
  }


  var featureDomainMap : immutable.Map[String, Int] = Map.empty


  def setFeatureDomain(domain: Map[String, Int]) {
      featureDomainMap = domain
  }

  //   println("Domain size: " + featureDomainMap.size)
  //   for ((feature, featureIdx) <- featureDomainMap) {
  //     println(feature + "\t" + featureIdx)
  //   }

  def entityToSvmFormat(mention: EntityMention, entity: ScoredWikipediaEntity, features: Map[String, Double]) : String = {
    var sb = new StringBuilder

    val target = if (mention.groundTruth equalsIgnoreCase entity.wikipediaTitle) 1 else 0
    sb append target
    sb append " "
    sb append "qid:"
    sb append mention.mentionId
    sb append " "

    for ((feature, value) <- features) {
      val domain = featureDomainMap.getOrElse(feature, -1) + 1
      // skip some features
      if (domain > 0) {
        sb append domain
        sb append ":"
        sb append value
        sb append " "
      }
    }
    //          val scoreComponentMap = queryComponents.getOrElse(result.docId, Seq[ContextFeature]())
    //          scoreComponentMap.map( sc => {
    //            val domain = featureDomainMap.getOrElse(sc.contextSource, -1) + 1
    //            if (domain > 0) {
    //              sb append domain
    //              sb append ":"
    //              sb append sc.score
    //              sb append " "
    //            }
    //          })

    sb append (featureDomainMap.size + 2) + ":"
    sb append (1.0 / (entity.rank + 1))
    sb append " "

    sb append (featureDomainMap.size + 3) + ":"
    sb append (entity.score)
    sb append " "

    sb append "#"
    sb append " "
    sb append entity.wikipediaTitle.trim
    //println(sb.toString)
    sb.toString

  }

}

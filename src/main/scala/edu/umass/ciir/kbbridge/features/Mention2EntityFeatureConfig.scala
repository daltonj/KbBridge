package edu.umass.ciir.kbbridge.features

import scala.collection.JavaConversions._

import edu.umass.ciir.kbbridge.data.{ScoredWikipediaEntity, WikipediaEntity, EntityMention}
import edu.umass.ciir.kbbridge.util.{RedisFeatureConnection, ConfInfo}
import edu.umass.ciir.kbbridge.features.GalagoEntityLinkingFeatureLib.GalagoEntityLinkingFeatures

//import cc.refectorie.user.dietz.tacco.features.mention2kbentity.WikifierEntityLinkingFeatureLib.WikifierEntityLinkingFeatures

/**
 * 
 */

object Mention2EntityFeatureHasher {
  val prefixSeparator = "."
  def featuresAsMap(featureConfig:Array[String], mention:EntityMention, entity:ScoredWikipediaEntity, otherCands:Seq[ScoredWikipediaEntity]):Map[String,  Double]  = {
    val featureMap = scala.collection.mutable.Map[String,  Double]()

    def addFeatureCall(prefix:String, category:String,  value:String){
      featureMap += (prefix+prefixSeparator+category+"="+value -> 1.0)
    }
    def addFeatureValueCall(prefix:String, category:String,  value:Double){
      featureMap += (prefix+prefixSeparator+category -> value)
    }

    val featureGen = new Mention2EntityFeatureConfig(featureConfig, addFeatureCall, addFeatureValueCall )
    featureGen.generateFeature(mention, entity, otherCands)

    featureMap.toMap
  }
}

trait Mention2EntityFeatureTrait {
  def generateFeature(mention:EntityMention, entity:ScoredWikipediaEntity, otherCands:Seq[ScoredWikipediaEntity])
}

trait Mention2EntityFeatureConfigurator extends Mention2EntityFeatureTrait with FeatureGenerator {
  def galago:Boolean
  def queryonly:Boolean
  def namevariants:Boolean
  def localdoc:Boolean  

  lazy val galagoFeatures = new FeatureSetup(addFeature, addValueFeature) with GalagoEntityLinkingFeatures{}
  lazy val queryOnlyFeatures = new FeatureSetup(addFeature, addValueFeature) with QueryOnlyFeatureGenerator{}
  lazy val localDocumentContext = new FeatureSetup(addFeature, addValueFeature) with LocalDocumentFeatures{}
  lazy val nameVariantsFeatures = new FeatureSetup(addFeature, addValueFeature) with NameVariantsFeatures{}

//  lazy val wikifierFeatures = new FeatureSetup(addFeature, addValueFeature) with WikifierEntityLinkingFeatures{}
  
  def generateFeature(mention:EntityMention, entity:ScoredWikipediaEntity, otherCands:Seq[ScoredWikipediaEntity]) {


    if(galago) galagoFeatures.generateGalagoEntityLinkingFeatures(mention, entity, otherCands)
    if(queryonly) queryOnlyFeatures.generateQueryOnlyFeatures(mention, entity, otherCands)
    if(namevariants) nameVariantsFeatures.documentContextNameVariantFeatures(mention, entity)
    if(localdoc) localDocumentContext.generateDocumentContextFeatures(mention, entity, otherCands)

  }
}

class Mention2EntityFeatureConfig(
    featureConfig:Array[String]
    , addFeatureCall:(String, String, String) => Unit
    ,  addFeatureValueCall:(String,String,Double) => Unit
  ) extends FeatureSetup(addFeatureCall, addFeatureValueCall) with Mention2EntityFeatureConfigurator {
  val galago = featureConfig.contains("galago")
  val queryonly = featureConfig.contains("queryonly")
  val namevariants = featureConfig.contains("namevar")
  val localdoc = featureConfig.contains("localcontext")
  

}

//class Mention2EntityRedisStorer(featureConfig:Array[String]) extends Mention2EntityFeatureTrait {
//  val jedisConnection = new RedisFeatureConnection(redisSvr = ConfInfo.redisFeatureSvr, redisPort = ConfInfo.redisFeaturePort, redisTimeout = ConfInfo.redisFeatureTimeout, database = ConfInfo.redisFeatureDB)
//  val redisFeatureSetName = ConfInfo.redisFeatureSetNameQuery2Entity+" # "+featureConfig.mkString(",")
//
//  def generateFeature(mention:EntityMention, entity:WikipediaEntity, otherCands:Seq[WikipediaEntity]){
//    val jedis = jedisConnection.jedis
//    val queryId = mention.mentionId
//    val entityId = entity.wikipediaTitle
//    val jedisKey = redisFeatureSetName+" # "+queryId+" # "+entityId
//
//    if(ConfInfo.redisFeatureOverwriteExisting || jedis.exists(jedisKey)){
//
//      val featureMap = Mention2EntityFeatureHasher.featuresAsMap(featureConfig, mention, entity, otherCands)
//      val stringFeatures = featureMap.map(entry => (entry._1, entry._2.toString))
//      val txOk = jedis.hmset(jedisKey, mapAsJavaMap(stringFeatures))
//      if(txOk != "OK") System.err.println("Writing features to redis failed because "+txOk)
//    }
//  }
//
//  def disconnect() = jedisConnection.disconnect()
//}



//object FeatureRedisConnectionPool extends RedisConnectionPool(redisSvr = ConfInfo.redisFeatureSvr, redisPort = ConfInfo.redisFeaturePort, redisTimeout = ConfInfo.redisFeatureTimeout, database = ConfInfo.redisFeatureDB, 10)
//
//class Mention2EntityRedisFeatureConfig(
//    featureConfig:Array[String]
//    , addFeatureCall:(String, String, String) => Unit
//    ,  addFeatureValueCall:(String,String,Double) => Unit
//  ) extends FeatureSetup(addFeatureCall, addFeatureValueCall) with Mention2EntityFeatureTrait {
//  val redisFeatureSetName = ConfInfo.redisFeatureSetNameQuery2Entity+" # "+featureConfig.mkString(",")
//
//  def generateFeature(mention:DataMention, entity:DataEntity, otherCands:Seq[DataEntity]) {
//    val fetchedRedisConnection = FeatureRedisConnectionPool.fetchRedis()
//    val jedis = fetchedRedisConnection.jedis
//    println("getting feature from redis for "+mention+" - "+entity)
//
//    val queryId = mention.asInstanceOf[TacELQuery].queryId
//    val entityId = entity.asInstanceOf[WikipediaEntity].wikipediaTitle
//    val stringFeatures =
//      jedis.hgetAll(redisFeatureSetName+" # "+queryId+" # "+entityId)
//    println(" found "+stringFeatures.size()+" features")
//
//    for((featureName,stringValue) <- stringFeatures.iterator) {
//
//      val featureKeys = featureName.split(Mention2EntityFeatureHasher.prefixSeparator)
//      val featurePrefix = featureKeys.head
//      val featureCategory = featureKeys.tail.mkString(Mention2EntityFeatureHasher.prefixSeparator)
//      val value = java.lang.Double.parseDouble(stringValue)
//      addFeatureValueCall(featurePrefix, featureCategory, value)
//    }
//    FeatureRedisConnectionPool.releaseRedis(fetchedRedisConnection)
//
//  }
//}




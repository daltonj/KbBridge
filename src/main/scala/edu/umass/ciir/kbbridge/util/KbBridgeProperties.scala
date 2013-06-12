package edu.umass.ciir.kbbridge.util

import java.util.Properties
import java.io.FileInputStream
import scala.collection.JavaConversions._

/**
 * User: jdalton
 * Date: 4/1/13
 */
object KbBridgeProperties {

  var conf : Properties = null
  val defaultPropertiesFile = "./config/kbbridge.properties"
  loadProperties(defaultPropertiesFile)

  def galagoJsonParameterFile = conf.getProperty("KbBridge.galagoKbJsonParameterFile")
  def useLocalIndex = conf.getProperty("KbBridge.useLocalIndex").toBoolean
  def galagoSrv = conf.getProperty("KbBridge.galagoKbSrv")
  def galagoKbaPort = conf.getProperty("KbBridge.galagoKbaPort")
  def numberOfRequestedResults = conf.getProperty("KbBridge.numberOfRequestedResults").toInt
  def numberFirstPassExpansionRMTerms = conf.getProperty("KbBridge.numberFirstPassExpansionRMTerms").toInt
  def numberSecondPassExpansionRMTerms = conf.getProperty("KbBridge.numberSecondPassExpansionRMTerms").toInt
  def useTwoPassWorkingSet = conf.getProperty("KbBridge.useTwoPassWorkingSet").toBoolean
  def useTaccoQuery = conf.getProperty("KbBridge.useTaccoQuery").toBoolean
  def rankerModelFile = conf.getProperty("KbBridge.rankerModelFile", "./data/ltr/models/tac_odd_allyears")
  def performNilPrediction = conf.getProperty("KbBridge.performNilPrediction", "false").toBoolean

  def loadProperties(propertiesFile : String) = {
    println("trying to load KbBridge properties from: " + propertiesFile)
    try {
      val properties = System.getProperties()
      val propStream = new FileInputStream(propertiesFile)
      properties.load(propStream)
      System.out.println("...loaded from "+propertiesFile)

      conf = properties
    } catch {
      case e => println("Unable to load file : " + propertiesFile + " " + e.getMessage())
      throw e
    }
  }

  def printProperties() {
    println("TrecKbaProperties:")

    val kbaProperties = conf.keys().toList.filterNot(e => (e.toString().startsWith("KbBridge")))
    for ( key <- conf.keys()) {
      if (key.toString().startsWith("KbBridge.")) {
        println(key + ":" + conf.getProperty(key.toString))
      }
    }
  }

}

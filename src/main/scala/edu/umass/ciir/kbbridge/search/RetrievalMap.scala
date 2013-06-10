package edu.umass.ciir.kbbridge.search

import collection.mutable
import edu.umass.ciir.kbbridge.util.ConfInfo

/**
 * User: jdalton
 * Date: 6/10/13
 */
object RetrievalMap {

  val searcherMap: mutable.Map[String, GalagoRetrieval] = new mutable.HashMap[String, GalagoRetrieval]()

  def getSearcher: GalagoRetrieval = {
    getSearcher("default")
  }

  def getSearcher(searcherName: String): GalagoRetrieval = {
    searcherMap.getOrElseUpdate(searcherName, {
      new GalagoRetrieval(ConfInfo.galagoJsonParameterFile, ConfInfo.galagoUseLocalIndex, ConfInfo.galagoSrv, ConfInfo.galagoPort)
    })
  }

  def getSearcher(searcherName: String, jsonConfigFile: String, galagoUseLocalIndex: Boolean, galagoSrv: String, galagoPort: String, candidateQueryType: String, resultLogFileName: String): GalagoRetrieval = {
    searcherMap.getOrElseUpdate(searcherName, {
      new GalagoRetrieval(jsonConfigFile, galagoUseLocalIndex, galagoSrv, galagoPort)
    })
  }



}

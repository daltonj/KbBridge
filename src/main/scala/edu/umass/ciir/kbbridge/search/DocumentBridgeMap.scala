package edu.umass.ciir.kbbridge.search

import collection.mutable
import edu.umass.ciir.kbbridge.util.ConfInfo
import edu.umass.ciir.kbbridge.data.DocumentProvider
import org.lemurproject.galago.tupleflow.Parameters

/**
 * User: jdalton
 * Date: 6/10/13
 */
object DocumentBridgeMap extends DocumentProvider {

  // todo add collection flag to the lookup, e.g. (identifier, wiki)

  def getDocument(identifier: String, params: Option[Parameters]) = {
    getDefaultProvider.getDocument(identifier, params)
  }

  def getPulledDocument(identifier: String, params: Option[Parameters]) = DocumentProvider.convertToPulledDocument(identifier, getDocument(identifier, params))

  def getFieldTermCount(cleanTerm: String, field: String) = getDefaultProvider.getFieldTermCount(cleanTerm, field)

  val searcherMap: mutable.Map[String, DocumentProvider] = new mutable.HashMap[String, DocumentProvider]()

  def getDefaultProvider: DocumentProvider = {
    getProvider("default")
  }

  def getProvider(searcherName: String): DocumentProvider = {
    searcherMap.getOrElseUpdate(searcherName, {
      new GalagoRetrieval(ConfInfo.galagoJsonParameterFile, ConfInfo.galagoUseLocalIndex, ConfInfo.galagoSrv, ConfInfo.galagoPort)
    })
  }

  def getProvider(searcherName: String, jsonConfigFile: String, galagoUseLocalIndex: Boolean, galagoSrv: String, galagoPort: String, candidateQueryType: String, resultLogFileName: String): DocumentProvider = {
    searcherMap.getOrElseUpdate(searcherName, {
      new GalagoRetrieval(jsonConfigFile, galagoUseLocalIndex, galagoSrv, galagoPort)
    })
  }





}

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
    getDefaultDocumentProvider.getDocument(identifier, params)
  }

  def getBridgeDocument(identifier: String, params: Option[Parameters]) = DocumentProvider.convertToBridgeDocument(identifier, getDocument(identifier, params))

  def getFieldTermCount(cleanTerm: String, field: String) = getDefaultDocumentProvider.getFieldTermCount(cleanTerm, field)

  val searcherMap: mutable.Map[String, GalagoRetrieval] = new mutable.HashMap[String, GalagoRetrieval]()

  def getKbRetrieval:GalagoRetrieval= {
    searcherMap.getOrElseUpdate("kb", {
      new GalagoRetrieval(ConfInfo.galagoKbJsonParameterFile, ConfInfo.galagoUseLocalIndex, ConfInfo.galagoKbSrv, ConfInfo.galagoKbPort)
    })
  }

  def getDefaultRetrieval: GalagoRetrieval= {
    searcherMap.getOrElseUpdate("default", {
      new GalagoRetrieval(ConfInfo.galagoDefaultJsonParameterFile, ConfInfo.galagoUseLocalIndex, ConfInfo.galagoDefaultSrv, ConfInfo.galagoDefaultPort)
    })
  }

  def getKbDocumentProvider:DocumentProvider = {
    getKbRetrieval
  }

  def getDefaultDocumentProvider: DocumentProvider = {
    getDefaultRetrieval
  }


  def fakeTokenize(text: String) = getDefaultRetrieval.fakeTokenize(text)

  private def getProvider(searcherName: String): Option[DocumentProvider] = {
    searcherMap.get(searcherName)
  }

  def getProvider(searcherName: String, jsonConfigFile: String, galagoUseLocalIndex: Boolean, galagoSrv: String, galagoPort: String, candidateQueryType: String, resultLogFileName: String): DocumentProvider = {
    searcherMap.getOrElseUpdate(searcherName, {
      new GalagoRetrieval(jsonConfigFile, galagoUseLocalIndex, galagoSrv, galagoPort)
    })
  }





}

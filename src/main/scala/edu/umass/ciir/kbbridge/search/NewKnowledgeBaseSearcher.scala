package edu.umass.ciir.kbbridge.search

import scala.collection.JavaConversions._
import edu.umass.ciir.memindex.Query
import java.util
import org.lemurproject.galago.core.tools.Search.SearchResultItem
import org.lemurproject.galago.core.tools.Search
import edu.umass.ciir.kbbridge.util.ConfInfo
import collection.mutable

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 12:09 PM
 */
class NewKnowledgeBaseSearcher(jsonConfigFile: String, galagoUseLocalIndex: Boolean, galagoSrv: String, galagoPort: String, candidateQueryType: String, resultLogFileName: String) {//extends KnowledgeBaseSearcherInterface {
  def search(query: Query, coreferentStrings: Seq[String], nerNeighbors: Seq[String]): Array[Search.SearchResultItem]= null

  def search(query: Query, coreferentStrings: Seq[String], nerNeighbors: Seq[String], sentences: Seq[String]): Array[Search.SearchResultItem] = null

  def searchComponents(query: Query, nameVariants: Seq[String], nerNeighbors: Map[String, Double], firstPassResults: Seq[SearchResultItem], sentences: Seq[String], uniformNer: Map[String, Double], localNer: Map[String, Double], discountNer: Map[String, Double], discountAdd: Map[String, Double]): Array[Search.SearchResultItem] = null

  def search(query: Query): Array[Search.SearchResultItem] = null

  def getDocument(docId: String, getTerms: Boolean): Search.SearchResultItem = null

  def getFieldTermCount(term: String, field: String):Long = 0L
}


object NewKnowledgeBaseSearcher{
  val searcherMap: mutable.Map[String, NewKnowledgeBaseSearcher] = new mutable.HashMap[String, NewKnowledgeBaseSearcher]()

  def getSearcher: NewKnowledgeBaseSearcher = {
    getSearcher("default")
  }

  def getSearcher(searcherName: String): NewKnowledgeBaseSearcher = {
    searcherMap.getOrElseUpdate(searcherName, {
      new NewKnowledgeBaseSearcher(ConfInfo.galagoJsonParameterFile, ConfInfo.galagoUseLocalIndex, ConfInfo.galagoSrv, ConfInfo.galagoPort, ConfInfo.candidateQueryType, ConfInfo.kbSearcherResultLogFile)
    })
  }

  def getSearcher(searcherName: String, jsonConfigFile: String, galagoUseLocalIndex: Boolean, galagoSrv: String, galagoPort: String, candidateQueryType: String, resultLogFileName: String): NewKnowledgeBaseSearcher = {
    searcherMap.getOrElseUpdate(searcherName, {
      new NewKnowledgeBaseSearcher(jsonConfigFile, galagoUseLocalIndex, galagoSrv, galagoPort, candidateQueryType, resultLogFileName)
    })
  }
}

class NewKnowledgeBaseSearcherAdapter(s:NewKnowledgeBaseSearcher) extends KnowledgeBaseSearcherInterface{
  def fixMap(map:util.Map[String,java.lang.Double]):Map[String,Double] = {
    map.map(pair => (pair._1, pair._2.toDouble)).toMap
  }

  def search(query: Query, coreferentStrings: util.List[String], nerNeighbors: util.List[String]) =  s.search(query, coreferentStrings, nerNeighbors)

  def search(query: Query, coreferentStrings: util.List[String], nerNeighbors: util.List[String], sentences: util.List[String]) =
  s.search(query, coreferentStrings, nerNeighbors, sentences)

  def searchComponents(query: Query, nameVariants: util.List[String], nerNeighbors: util.Map[String, java.lang.Double], firstPassResults: util.List[SearchResultItem], sentences: util.List[String], uniformNer: util.Map[String, java.lang.Double], localNer: util.Map[String, java.lang.Double], discountNer: util.Map[String, java.lang.Double], discountAdd: util.Map[String, java.lang.Double]) =
    s.searchComponents(query, nameVariants, fixMap(nerNeighbors), firstPassResults, sentences, fixMap(uniformNer), fixMap(localNer), fixMap(discountNer), fixMap(discountAdd))

  def search(query: Query) = s.search(query)

  def getDocument(docId: String, getTerms: Boolean) = s.getDocument(docId, getTerms)

  def getFieldTermCount(term: String, field: String) = s.getFieldTermCount(term, field)
}
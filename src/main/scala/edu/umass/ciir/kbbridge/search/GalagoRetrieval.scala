package edu.umass.ciir.kbbridge.search

import java.io.File
import org.lemurproject.galago.core.retrieval.query.{AnnotatedNode, StructuredQuery, Node}
import org.lemurproject.galago.tupleflow.{FakeParameters, Parameters}
import org.lemurproject.galago.core.parse.{TagTokenizer, Document}
import scala.collection.JavaConversions._
import org.lemurproject.galago.core.retrieval.{RetrievalFactory, ScoredPassage, ScoredDocument}
import org.lemurproject.galago.core.scoring.WeightedTerm
import scala._
import com.google.common.cache.{CacheLoader, CacheBuilder, LoadingCache}
import org.lemurproject.galago.core.index.AggregateReader.NodeStatistics
import java.util.concurrent.TimeUnit
import edu.umass.ciir.kbbridge.data.{GalagoBridgeDocumentWrapper, GalagoBridgeDocument, DocumentProvider}

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 12:08 PM
 */

class GalagoRetrieval(jsonConfigFile: String, galagoUseLocalIndex: Boolean, galagoSrv: String = "", galagoPort: String="", val usePassage:Boolean = false) extends DocumentProvider {

  val termStatisticsCache: LoadingCache[String, NodeStatistics] = CacheBuilder.newBuilder()
    .maximumSize(1000)
    .expireAfterWrite(10, TimeUnit.MINUTES)
    .build(
    new CacheLoader[String, NodeStatistics]() {
      def load(key: String): NodeStatistics = {
        return getStatistics(key);
      }
    });

  val globalParameters = Parameters.parse(new File(jsonConfigFile))

  if (!galagoUseLocalIndex) {
    val remoteIndex = "http://" + galagoSrv + ":" + galagoPort
    globalParameters.set("index", remoteIndex)
  }
  if (globalParameters.isString("index")) println("** Loading index from: " + globalParameters.getString("index"))

  val queryParams = new Parameters
  val defaultSmoothingMu = globalParameters.getDouble("defaultSmoothingMu")
  val m_searcher = RetrievalFactory.instance(globalParameters)



  def rmExpansion(rawQuery: String, queryParams: Parameters): Seq[WeightedTerm] = {
    val rmR = new RelevanceModelExpander(m_searcher, queryParams)
    rmR.runExpansion(rawQuery, queryParams)
  }

    def getDocuments(identifier:Seq[String],  params:Option[Parameters] = None):Map[String, Document] = {
      val p = new Parameters()
      p.copyFrom(globalParameters)
      params match {case Some(param) => p.copyFrom(param); case _ => {}}
      p.set("terms", true)
      p.set("tags", true)
      try {
        m_searcher synchronized {
          val docmap = m_searcher.getDocuments(identifier, p)
          docmap.toMap
        }
      } catch {
        case ex: NullPointerException => {
          println("NPE while fetching documents " + identifier)
          throw ex
        }
      }
    }

  def getDocument(identifier:String,  params:Option[Parameters] = None): Document = {
    val p = new Parameters()
    p.copyFrom(globalParameters)
    params match {case Some(param) => p.copyFrom(param); case _ => {}}
    p.set("terms", true)
    p.set("tags", true)
    try {
      m_searcher synchronized {
        val doc = m_searcher.getDocument(identifier, p)
        if(doc == null) throw new BridgeDocumentNotFoundException(identifier)
        doc
      }
    } catch {
      case ex: NullPointerException => {
        throw new BridgeDocumentNotFoundException(identifier)
      }
      case ex => throw ex
    }
  }


  def convertScoredDocToGalagoBridgeDoc(sd:ScoredDocument):GalagoBridgeDocument = {
    new GalagoBridgeDocumentWrapper(sd.documentName, rawScore = Some(sd.score), rank = Some(sd.rank), galagoDocument = None)
  }

  def getStatistics(query: String): NodeStatistics = {
    println("fetching stats: " + query)
    m_searcher synchronized {
      try {
        val r = m_searcher
        val root = StructuredQuery.parse(query)
        root.getNodeParameters.set("queryType", "count")
        val transformed = r.transformQuery(root, queryParams)
        r.getNodeStatistics(transformed)
      } catch {
        case e: Exception => {
          println("Error getting statistics for query: " + query)
          throw e
        }
      }
    }
  }

  def getBridgeDocument(identifier: String,  params:Option[Parameters] = None) = DocumentProvider.convertToBridgeDocument(identifier, getDocument(identifier, params))

  def getFieldTermCount(cleanTerm: String, field: String): Long = {
    if (cleanTerm.length > 0) {
      val transformedText = "\"" + cleanTerm + "\"" + "." + field
      val statistics = termStatisticsCache.get(transformedText);
      //val statistics = getStatistics(transformedText)
      statistics.nodeFrequency
    } else {
      0
    }
  }

  def retrieveAnnotatedScoredDocuments(query: String, params: Parameters, resultCount: Int, debugQuery: ((Node, Node) => Unit) = ((x, y) => {})): Seq[(ScoredDocument, AnnotatedNode)] = {
    params.set("annotate", true)
    for (scoredAnnotatedDoc <- retrieveScoredDocuments(query, resultCount, Some(params), debugQuery)) yield {
      (scoredAnnotatedDoc, scoredAnnotatedDoc.annotation)
    }
  }

  def retrieveScoredDocuments(query: String, resultCount: Int, params: Option[Parameters] = None, debugQuery: ((Node, Node) => Unit) = ((x, y) => {})): Seq[ScoredDocument] = {
    //    val begin = System.currentTimeMillis()
    try {
    val p = new Parameters()
    p.copyFrom(globalParameters)
    params match {
      case Some(params) => p.copyFrom(params)
      case None => {}
    }
    p.set("startAt", 0)
    p.set("resultCount", resultCount)
    p.set("requested", resultCount)
    val root = StructuredQuery.parse(query)
    val transformed = m_searcher.transformQuery(root, p)
    debugQuery(root, transformed)
    val result = m_searcher.runQuery(transformed, p)
    if (result == null) {
      println("Null results for query: " + query)
      Seq()
    } else {
      result
    }
    //    println("@retrieveScoredDocuments took "+TimeTools.printTimeDifference(System.currentTimeMillis() - begin)+"\tjsonConfigFile = "+jsonConfigFile)//+ " params: "+params.toPrettyString)

    } catch {
      case ex: Exception => println("ERROR running query: " + query + " " + ex.getMessage)
      throw ex
    }
  }

  def retrieveScoredPassages(query: String, params: Parameters, resultCount: Int, debugQuery: ((Node, Node) => Unit) = ((x, y) => {})): Seq[ScoredPassage] = {
    retrieveScoredDocuments(query, resultCount, Some(params), debugQuery).map(_.asInstanceOf[ScoredPassage])
  }

  def retrieveAnnotatedScoredPassages(query: String, params: Parameters, resultCount: Int, debugQuery: ((Node, Node) => Unit) = ((x, y) => {})): Seq[(ScoredPassage, AnnotatedNode)] = {
    params.set("annotate", true)
    for (scoredAnnotatedDoc <- retrieveScoredDocuments(query, resultCount, Some(params), debugQuery)) yield {
      (scoredAnnotatedDoc.asInstanceOf[ScoredPassage], scoredAnnotatedDoc.annotation)
    }
  }

  /**
   * Maintains the order of the search results but augments them with Document instances
   * @param resultList
   * @return
   */
  def fetchDocuments(resultList: Seq[ScoredDocument]): Seq[FetchedScoredDocument] = {
    val docNames = resultList.map(_.documentName)
    val docs = getDocuments(docNames)
    for (scoredDoc <- resultList) yield {
      FetchedScoredDocument(scoredDoc,
        docs.getOrElse(scoredDoc.documentName, {
          throw new DocumentNotInIndexException(scoredDoc.documentName)
        })
      )
    }
  }

  /**
   * Maintains the order of the search results but augments them with Document instances
   * @param resultList
   * @return
   */
  def fetchPassages(resultList: Seq[ScoredPassage]): Seq[FetchedScoredPassage] = {
    val docNames = resultList.map(_.documentName)
    val docs = getDocuments(docNames)
    for (scoredPassage <- resultList) yield {
      FetchedScoredPassage(scoredPassage,
        docs.getOrElse(scoredPassage.documentName, {
          throw new DocumentNotInIndexException(scoredPassage.documentName)
        })
      )
    }
  }


  def fakeTokenize(text: String): Document = {
    val tagTokenizer = new TagTokenizer(new FakeParameters(globalParameters))
    tagTokenizer.tokenize(text)
  }

  def close() {
  }
}

case class FetchedScoredDocument(scored: ScoredDocument, doc: Document)

case class FetchedScoredPassage(scored: ScoredPassage, doc: Document)

class DocumentNotInIndexException(val docName: String) extends RuntimeException





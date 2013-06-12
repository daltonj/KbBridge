package edu.umass.ciir.kbbridge.data

import org.lemurproject.galago.core.parse.Document
import org.lemurproject.galago.tupleflow.Parameters

/**
 * User: dietz
 * Date: 6/10/13
 * Time: 4:30 PM
 */
trait DocumentProvider {
  def getDocument(identifier:String, params:Option[Parameters] = None):Document
  def getBridgeDocument(identifier:String, params:Option[Parameters] = None):BridgeDocument
  def getFieldTermCount(cleanTerm:String, field: String): Long
}

object DocumentProvider {
  def convertToPulledDocument(identifier: String, galagoDocument:Document,  params:Option[Parameters] = None):BridgeDocument = {
    new GalagoBridgeDocumentWrapper(identifier, galagoDocument = Some(galagoDocument:Document))
  }


}

trait BridgeDocument{
  def documentname:String
  def metadata: Map[String, String]
  def text:String
  def terms: Seq[String]
}

trait ScoredBridgeDocument extends BridgeDocument {
  def rank:Option[Int]
  def rawScore:Option[Double]
  def relevanceScore:Option[Double]
}

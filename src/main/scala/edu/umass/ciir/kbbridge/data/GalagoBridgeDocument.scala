package edu.umass.ciir.kbbridge.data

import scala.collection.JavaConversions._
/**
 * User: dietz
 * Date: 6/7/13
 * Time: 5:23 PM
 */
trait GalagoBridgeDocument extends ScoredBridgeDocument{
  def documentname:String
  def rawScore:Option[Double]
  def relevanceScore:Option[Double]
  def rank:Option[Int]
  def galagoDocument: Option[org.lemurproject.galago.core.parse.Document]

  /** Same thing just without the galago document (and therefore no access to text or metadata)*/
  def dropDocument:GalagoBridgeDocument
}

class GalagoBridgeDocumentWrapper(val documentname:String, val rawScore:Option[Double]=None, val relevanceScore:Option[Double]=None, val rank:Option[Int]=None, val galagoDocument: Option[org.lemurproject.galago.core.parse.Document]) extends ScoredBridgeDocument with GalagoBridgeDocument {
  def metadata: Map[String, String] = {
    if(galagoDocument.isEmpty) Map.empty
    else {
      val m = galagoDocument.get.metadata
      if (m == null) Map.empty
      else m.toMap
    }
  }
  def text:String = {
    if(galagoDocument.isEmpty) ""
    else {
      val t = galagoDocument.get.text
      if (t == null) ""
      else t
    }
  }

  def terms: Seq[String] = {
    if(galagoDocument.isEmpty) Seq.empty[String]
    else {
      val t = galagoDocument.get.terms
      if (t == null) Seq.empty[String]
      else t
    }
  }

  def dropDocument:GalagoBridgeDocument = {
    if(galagoDocument.isEmpty) this.asInstanceOf[GalagoBridgeDocument]
    else new GalagoBridgeDocumentWrapper(documentname, rawScore, relevanceScore, rank, None).asInstanceOf[GalagoBridgeDocument]
  }

  def ressurectDocument(docProvider:DocumentProvider):GalagoBridgeDocument = {
    if(galagoDocument.isDefined) this.asInstanceOf[GalagoBridgeDocument]
    else {
      val galagoDoc = docProvider.getDocument(documentname)
      new GalagoBridgeDocumentWrapper(documentname, rawScore, relevanceScore, rank, Some(galagoDoc)).asInstanceOf[GalagoBridgeDocument]
    }
  }
}


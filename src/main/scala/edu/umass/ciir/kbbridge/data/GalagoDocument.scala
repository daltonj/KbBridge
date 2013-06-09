package edu.umass.ciir.kbbridge.data

import scala.collection.JavaConversions._
/**
 * User: dietz
 * Date: 6/7/13
 * Time: 5:23 PM
 */
class GalagoDocument(val documentname:String, val score:Double, val rank:Int, val galagoDocument: org.lemurproject.galago.core.parse.Document) {
  def metadata: Map[String, String] = {
    val m = galagoDocument.metadata
    if (m == null) Map.empty
    else m.toMap
  }
  def text:String = {
    val t = galagoDocument.text
    if (t == null) ""
    else t
  }

  def terms: Seq[String] = {
    val t = galagoDocument.terms
    if (t == null) Seq.empty[String]
    else t
  }


}
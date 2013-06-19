package edu.umass.ciir.kbbridge.kb2text

import edu.umass.ciir.kbbridge.data.BridgeDocument

/**
 * User: dietz
 * Date: 6/11/13
 * Time: 6:16 PM
 */
class TrecKbaDocument(documentId:String, topicId:String, rank:Int, rawScore:Option[Double]=None, relevanceScore:Option[Double]=None) {
  val docStreamTime:Long = documentId.split("-")(0).toLong

}

package edu.umass.ciir.kbbridge.data

/**
 * User: jdalton
 * Date: 3/29/13
 */
trait DocumentTextSource {
  def fullText (docId:String):String
}

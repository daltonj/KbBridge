package edu.umass.ciir.kbbridge.util

import org.lemurproject.galago.core.parse.Document

object WikiXmlTextExtractor {

  def extractText(galagoEnitityDoc : Document):String = {
    val textLen = "<text>".length()
    val text = galagoEnitityDoc.text
    val begin = text.indexOf("<text>")
    val end = text.indexOf("</text>",begin)

    val extractedText =
      if(begin>=0 && end>=0)
       text.substring(begin+textLen,end)
      else {
        System.err.println("Can't find text-field for galago document "+galagoEnitityDoc.identifier)
        ""
      }
    extractedText
  }
}
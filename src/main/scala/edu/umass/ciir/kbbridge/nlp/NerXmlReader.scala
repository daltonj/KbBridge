package edu.umass.ciir.kbbridge.nlp

import xml.XML
import collection.mutable.ListBuffer
import org.xml.sax.SAXParseException
import java.io.File
import edu.umass.ciir.kbbridge.nlp.NlpData.{Token, NlpXmlMention}

/**
 * 
 */

object NlpExtractReader {

  /**
   * Returns sequence of (NERType, text), where multi-word expressions are concatenated in text (with spaces)
   */
  def getNers(nlpFile:File):Seq[(String,  String)]={
    for ((ner, mention) <- getNersDetails(nlpFile)) yield {
      (ner, mention.text)
    }
  }
  def getNersDetails(nlpFile:File):Seq[(String,  NlpXmlMention)]={

    var currMention = ""
    var currTokens = ListBuffer[Token]()
    var currNerType = "O"
    var lastTokenIdx = -1
    val mentions = ListBuffer[(String, NlpXmlMention)]()
//    val mentions = ListBuffer[(String,String)]()

    val xmlReader = new java.io.InputStreamReader(new java.io.FileInputStream(nlpFile), "ISO-8859-1")
    try {
      val docNode = XML.load(reader = xmlReader) \\ "document" \ "sentences"
      for(s <- docNode \ "sentence")  {
//      for(s <- XML.load(reader = xmlReader) \\ "sentence")  {
        val sentenceId = Integer.parseInt(s.attribute("id").get.text)
        for((t, tokenIdx) <- (s \\ "token").zipWithIndex)  {
//          val ner = (t \ "NER").text
//          val word = (t \ "word").text
//
//          val tokenId = Integer.parseInt(t.attribute("id").get.text)
            val ner = (t \ "NER").text
            val word = (t \ "word").text
            val pos = (t \ "POS").text
            val lemma = (t \ "lemma").text
            val charBegin = Integer.parseInt((t \ "CharacterOffsetBegin").text)
            val charEnd = Integer.parseInt((t \ "CharacterOffsetEnd").text)
            val token =  Token(rawWord = word, ner = ner, pos=pos, lemma=lemma,charBegin=charBegin, charEnd=charEnd)


          if (ner != "O") {

            if (currNerType == ner) {
              if (currMention != "") currMention += " "
              currMention += word
              currTokens += token
            } else {
              if (currMention != "") {
                // submit previous mention
                assert(currNerType != "O",{"error case 1: currMention="+currMention})
                mentions += (currNerType -> new NlpXmlMention(currMention, currTokens, sentenceId, true, tokenIdx-1-currTokens.length, tokenIdx-1, currTokens.head.charBegin, currTokens.last.charEnd))
              }
              // start new mention
              currMention = word
              currTokens = new ListBuffer[Token]()
              currTokens += token
            }


          } else {
            if (currMention != "") {
              assert(currNerType != "O",{"error case 2: currMention="+currMention})
//              mentions += (currNerType -> currMention)
              mentions += (currNerType -> new NlpXmlMention(currMention, currTokens, sentenceId, true, tokenIdx-1-currTokens.length, tokenIdx-1, currTokens.head.charBegin, currTokens.last.charEnd))
              currMention = ""
              currTokens = new ListBuffer[Token]()
            }

          }
          currNerType = ner
          lastTokenIdx = tokenIdx
        }
        if (currMention != "") {
          assert(currNerType != "O",{"error case 3: currMention="+currMention})
          mentions += (currNerType -> new NlpXmlMention(currMention, currTokens, sentenceId, true, lastTokenIdx-currTokens.length, lastTokenIdx, currTokens.head.charBegin, currTokens.last.charEnd))
          currMention = ""
          currTokens = new ListBuffer[Token]()
        }
        currNerType = "O"
      }


      mentions
    } catch {
      case ex:SAXParseException => {
        System.err.println("Sax exception when parsing NERs from "+nlpFile.getAbsolutePath)
//        for((line, no) <- io.Source.fromFile(nlpFile, "ISO-8859-1").getLines.zipWithIndex){
//          println(no+": "+line)
//        }
        throw ex
      }
    } finally {
      xmlReader.close()
    }
  }

}
object NlpExtractReaderTest  {
  def main(args:Array[String]){
    val res = NlpExtractReader.getNers(new File("/Volumes/blake//iesl/canvas/dietz/tacnlpextract/stanf-harshal/tac/APW_ENG_20070813.1111.LDC2009T13.xml"))
//    val res = NlpExtractReader.getNers(new File("/Users/dietz/tac-kbp2011/data/extractStanf/tac/eng-WL-11-174596-12951106.xml"))
//    val res = NlpExtractReader.getNers(new File("/Users/dietz/tac-kbp2011/data/extractStanf/xx.xml"))
    for((t,w) <- res) {
      println(t+" -> "+w)
    }
  }
}
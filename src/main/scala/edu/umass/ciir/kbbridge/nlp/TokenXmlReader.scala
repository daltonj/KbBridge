package edu.umass.ciir.kbbridge.nlp

import xml.XML
import java.io.File
import collection.mutable.ListBuffer
import org.xml.sax.SAXParseException
import edu.umass.ciir.kbbridge.nlp.NlpData.Token

/**
 * 
 */

object TokenXmlReader {
  case class Sentence(sentenceId:Int,  tokens:Seq[Token])  {
    def text:String = tokens.map(_.word).mkString(" ")
  }

  def getSentences(nlpFile:File):Seq[Sentence]={
  val xmlReader = new java.io.InputStreamReader(new java.io.FileInputStream(nlpFile), "ISO-8859-1")
  try {
    val docNode = XML.load(reader = xmlReader) \\ "document" \ "sentences"
    val sentences = ListBuffer[Sentence]()

    for(s <- docNode \ "sentence")  {
      val sentenceId = Integer.parseInt(s.attribute("id").get.text)
      val tokens =
        for((t, tokenIdx) <- (s \\ "token").zipWithIndex.toSeq) yield {
          val ner = (t \ "NER").text
          val word = (t \ "word").text
          val pos = (t \ "POS").text
          val lemma = (t \ "lemma").text
          val charBegin = Integer.parseInt((t \ "CharacterOffsetBegin").text)
          val charEnd = Integer.parseInt((t \ "CharacterOffsetEnd").text)
          val token =  Token(rawWord = word, ner = ner, pos=pos, lemma=lemma,charBegin=charBegin, charEnd=charEnd)
          token
      }
      sentences += Sentence(sentenceId, tokens)
    }

    sentences
  } catch {
    case ex:SAXParseException => {
      System.err.println("Sax exception when parsing NERs from "+nlpFile.getAbsolutePath)
      throw ex
    }
  } finally {
    xmlReader.close()
  }

  }
}

object TokenXmlReaderTest  {
  def main(args:Array[String]){
    val res = TokenXmlReader.getSentences(new File("/Volumes/blake//iesl/canvas/dietz/tacnlpextract/stanf-harshal/tac/APW_ENG_20070813.1111.LDC2009T13.xml"))
//    val res = NlpExtractReader.getNers(new File("/Users/dietz/tac-kbp2011/data/extractStanf/tac/eng-WL-11-174596-12951106.xml"))
//    val res = NlpExtractReader.getNers(new File("/Users/dietz/tac-kbp2011/data/extractStanf/xx.xml"))
    for(sent <- res) {
      println(sent)
    }
  }
}
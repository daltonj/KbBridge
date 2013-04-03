package edu.umass.ciir.kbbridge.nlp

import java.io.File
import xml.XML
import org.xml.sax.SAXParseException
import collection.mutable.{HashMap, ListBuffer}
import edu.umass.ciir.kbbridge.nlp.NlpData.{Token, NlpXmlMention}

/**
 * 
 */

object CorefXmlReader {
  private case class PreMention(sentenceId:Int, start:Int,  end:Int, isRepresentative:Boolean = false)


  def getCorefChains(nlpFile:File):Seq[Seq[NlpXmlMention]] = {
    val xmlReader1 = new java.io.InputStreamReader(new java.io.FileInputStream(nlpFile), "ISO-8859-1")
    val xmlReader2 = new java.io.InputStreamReader(new java.io.FileInputStream(nlpFile), "ISO-8859-1")
    try {
      val prechains =
        for(corefChain <- (XML.load(reader = xmlReader1) \\ "coreference"); if( !(corefChain \ "mention").isEmpty))  yield {

          for (mention <- corefChain \ "mention") yield {
            val isRepresentative  =
              mention.attribute("representative") match {
                case None => false
                case Some(text) => text.text equalsIgnoreCase "true"
              }
            val sentenceId = Integer.parseInt((mention \ "sentence").text)
            val start = Integer.parseInt((mention \ "start").text)
            val end = Integer.parseInt((mention \ "end").text)

            PreMention(sentenceId = sentenceId, start=start, end=end, isRepresentative = isRepresentative)
          }
        }

      val wantedTokens = new HashMap[Int,  scala.collection.mutable.Set[Int]]()
      for(seq <- prechains; pm <- seq) yield {
        val tokens = wantedTokens.getOrElseUpdate(pm.sentenceId, {scala.collection.mutable.Set[Int]()})
        for(i <- pm.start until pm.end) {
          tokens += i
        }
      }

      val tokenMap = new HashMap[(Int, Int), Token]
      val docNode = XML.load(reader = xmlReader2) \\ "document" \ "sentences"
      for(s <- docNode \ "sentence")  {
        val sentenceId = Integer.parseInt(s.attribute("id").get.text)
        wantedTokens.get(sentenceId) match {
          case None => {}
          case Some(setOfIds) => {

            for(t <- s \\ "token")  {
              val tokenId = Integer.parseInt(t.attribute("id").get.text)
              if (setOfIds.contains(tokenId)) {
                val ner = (t \ "NER").text
                val word = (t \ "word").text
                val pos = (t \ "POS").text
                val lemma = (t \ "lemma").text
                val charBegin = Integer.parseInt((t \ "CharacterOffsetBegin").text)
                val charEnd = Integer.parseInt((t \ "CharacterOffsetEnd").text)
                tokenMap.update( (sentenceId, tokenId), Token(rawWord = word, ner = ner, pos=pos, lemma=lemma,charBegin=charBegin, charEnd=charEnd))
              }
            }
          }
        }
      }



//      val wantedkeys = wantedTokens.toSeq.flatMap( entry => entry._2.map(tok => (entry._1, tok))).toSet
//      val gotkeys = tokenMap.keySet
//      println("found "+(wantedkeys intersect gotkeys).size+ " wanted keys out of "+wantedkeys.size)


      val corefChains =
        for(chain <- prechains) yield {
          for(pm <- chain) yield {
            var text = ""
            var charBegin = -1
            var charEnd = -1
//            val nerSeq = ListBuffer[String]()
            val tokens = ListBuffer[Token]()
            for(i <- pm.start until pm.end){
              val token = tokenMap(Pair(pm.sentenceId, i))
              tokens += token
              if (text.length >0) text += " "
              if (charBegin < 0 ) charBegin = token.charBegin
              charEnd = token.charEnd            
              text += token.word
//              nerSeq += token.ner
            }
            new NlpXmlMention(text = text, tokens = tokens, sentenceId = pm.sentenceId, isRepresentative = pm.isRepresentative
              ,tokenStart = pm.start, tokenEnd = pm.end, charBegin = charBegin, charEnd = charEnd)
          }
        }
      corefChains



    } catch {
      case ex:SAXParseException => {
        System.err.println("Sax exception when parsing NERs from "+nlpFile.getAbsolutePath)
        throw ex
      }
    } finally {
      xmlReader1.close()
      xmlReader2.close()
    }
  }
}

object CorefXmlReaderTest {
  def main(args:Array[String]) {
    val chains = CorefXmlReader.getCorefChains(new File("/Volumes/blake//iesl/canvas/dietz/tacnlpextract/stanf-harshal/tac/NYT_ENG_20070720.0079.LDC2009T13.xml"))
//    val chains = CorefXmlReader.getCorefChains(new File("/Users/dietz/tac-kbp2011/data/extractStanf/eng-WL-11-174584-12960463.xml"))

    for(chain <- chains){
      println("chain: ")
      for(mention <- chain) {
        println(mention)
      }
      println()
    }


  }
}
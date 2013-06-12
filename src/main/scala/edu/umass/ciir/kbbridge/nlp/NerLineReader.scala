package edu.umass.ciir.kbbridge.nlp

import collection.mutable.ListBuffer
import scala.io.Source

import edu.umass.ciir.kbbridge.nlp.TokenXmlReader.Sentence
import edu.umass.ciir.kbbridge.nlp.NlpData.{NlpXmlNerMention, Token}
;

object NerLineReader {

   /**
   * Returns sequence of (NERType, text), where multi-word expressions are concatenated in text (with spaces)
   */
  def getNers(nerString : String):Seq[(String,  String)]={
    for ((ner, mention) <- extractNerMentions(nerString)) yield {
      (ner, mention.text)
    }
  }
  
  def extractNerMentions(nerString : String) : Seq[(String,  NlpXmlNerMention)] = {
    
    var currMention = ""
    var currTokens = ListBuffer[Token]()
    var currNerType = "O"
    var lastTokenIdx = -1
    val mentions = ListBuffer[(String, NlpXmlNerMention)]()
    var sentenceId = 1;
    
    var tokens = ListBuffer[Token]()
    
    val source = Source.fromString(nerString)
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        tokens = new ListBuffer[Token]()
        sentenceId += 1
      } else {

        val fields = line.split("\t")
        if (fields.length != 7) {
            System.out.println("num fields: " + fields.length + " " + line)
        } else {
        val tokenIdx = fields(0).toInt
        val word = fields(1)
        val trueCased = fields(2)
        val partOfSpeech = fields(3)
        val ner = fields(4)
        val charBegin = fields(5).toInt
        val charEnd = fields(6).stripLineEnd.toInt
        val token =  Token(rawWord = word, ner = ner, pos=partOfSpeech, lemma=trueCased, charBegin=charBegin, charEnd=charEnd)
        
        // NOTE: Below is taken from NerXmlReader in TACCO
         if (ner != "O") {
            if (currNerType == ner) {
              if (currMention != "") currMention += " "
              currMention += word
              currTokens += token
            } else {
              if (currMention != "") {
                // submit previous mention
                assert(currNerType != "O",{"error case 1: currMention="+currMention})
                mentions += (currNerType -> new NlpXmlNerMention(currMention, currTokens, sentenceId, true, tokenIdx-1-currTokens.length, tokenIdx-1, currTokens.head.charBegin, currTokens.last.charEnd, currNerType))
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
              mentions += (currNerType -> new NlpXmlNerMention(currMention, currTokens, sentenceId, true, tokenIdx-1-currTokens.length, tokenIdx-1, currTokens.head.charBegin, currTokens.last.charEnd, currNerType))
              currMention = ""
              currTokens = new ListBuffer[Token]()
            }

          }
          currNerType = ner
          lastTokenIdx = tokenIdx
        }
       }
      }
      if (currMention != "") {
          assert(currNerType != "O",{"error case 3: currMention="+currMention})
          mentions += (currNerType -> new NlpXmlNerMention(currMention, currTokens, sentenceId, true, lastTokenIdx-currTokens.length, lastTokenIdx, currTokens.head.charBegin, currTokens.last.charEnd, currNerType))
          currMention = ""
          currTokens = new ListBuffer[Token]()
      }
      currNerType = "O"
    mentions
  }
  
   def extractSentencesFromNerData(nerString : String) : Seq[Sentence] = {
    
    val sentences = ListBuffer[Sentence]()
    val source = Source.fromString(nerString)
    
    var tokens = ListBuffer[Token]()
    
    var sentenceId = 1;
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        sentences += Sentence(sentenceId, tokens)
        tokens = new ListBuffer[Token]()
        sentenceId += 1
      } else {
        val fields = line.split("\t")
        if (fields.length != 7) {
          System.out.println("num fields: " + fields.length + " " + line)
        } else {
        val sentencePos = fields(0)
        val rawWord = fields(1)
        val trueCased = fields(2)
        val partOfSpeech = fields(3)
        val ner = fields(4)
        val charBegin = fields(5).toInt
        val charEnd = fields(6).stripLineEnd.toInt
        val token =  Token(rawWord = rawWord, ner = ner, pos=partOfSpeech, lemma=trueCased, charBegin=charBegin, charEnd=charEnd)
        tokens += token
         }
     }
   }
   sentences += Sentence(sentenceId, tokens) 
   sentences
  }
   
  
//  def main(args: Array[String]) {
//    
//     TrecKbaProperties.loadProperties("./config/trec-kba.properties")
//
//     val kbaSearcher = KnowledgeBaseSearcher.getSearcher("kba", TrecKbaProperties.galagoKbJsonParameterFile, TrecKbaProperties.useLocalIndex, TrecKbaProperties.galagoKbSrv, TrecKbaProperties.galagoKbaPort, "kba", "kba")
//  
//     val acceptableNerTypes = Set("PERSON", "LOCATION", "ORGANIZATION", "UNK")
//
//     val p1 = new Parameters();
//     p1.set("terms", true);
//     p1.set("tags", false);
//     
//     val doc = kbaSearcher.getUnderlyingSearcher().getDocument("1318074334-f7477669dedc1236f91682c11bf6fba6", p1);
//     val nerData = doc.metadata.get("nerData")
//     println(nerData)
//     val ners = NerLineReader.extractNerMentions(nerData).map(pair => pair._2)
//     var filteredNers = ners.filter ( m  => acceptableNerTypes.contains(m.ner)) 
//     for (ner <- filteredNers) {
//       println(ner.text)
//     }
//  }
}
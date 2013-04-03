package edu.umass.ciir.kbbridge.nlp

import collection.mutable.ListBuffer
import java.io.File
import edu.umass.ciir.kbbridge.nlp.NlpData.{Token, NlpXmlNerMention}
import edu.umass.ciir.kbbridge.data.EntityMention


object NlpReader {


//  def allCorefNers( fullText: () => String, docId:String,  source:String): scala.Seq[(String, NlpXmlMention)] = {
//    var nlpFile = NlpExtractor.getOrQueueNlp(fullText, docId, source)
//    val ners = NlpExtractReader.getNersDetails(nlpFile.get)
//    ners
//  }


  def getNerSpans(query: EntityMention) : scala.Seq[scala.Seq[NlpXmlNerMention]] = {
    
//    if (query.isInstanceOf[BookELQueryImpl]) {
//      val bookQuery = query.asInstanceOf[BookELQueryImpl]
//      val ners = bookQuery.nerContext
//      val corefs = ners.groupBy(_.text).values
//      corefs.toSeq
//    } else if (query.isInstanceOf[KbaQueryNerMention]) {
//      val kbaSearcher = KnowledgeBaseSearcher.getSearcher("kba")
//      val r = kbaSearcher.getDocument(query.docId, true)
//      if (r != null) {
//         val nerData = r.metadata.get("nerData")
//         val ners = NerLineReader.extractNerMentions(nerData).map(pair => pair._2)
//         val corefs = ners.groupBy(_.text).values
//         corefs.toSeq
//      } else {
//        Seq()
//      }
//
//
//    } else {
      val nlpFile = NlpExtractor.getOrQueueNlp(() => {query.fullText}, query.docId, "tac")
      nlpFile match {
        case None => PoorMansNlpExtractor.poorMansNerSpanDetector(query.fullText)
        case Some(filename) => allCorefWithSingletonNers(filename)
      }
//    }
   
  }


  def allCorefWithSingletonNers(nlpFile:File): scala.Seq[scala.Seq[NlpXmlNerMention]] = {


//  def allCorefWithSingletonNers( fullText: () => String, docId:String,  source:String): scala.Seq[scala.Seq[NlpXmlNerMention]] = {
//    val nlpFile = NlpExtractor.getOrQueueNlp(fullText, docId, source)
    val ners = NlpExtractReader.getNersDetails(nlpFile)
    val corefChains = CorefXmlReader.getCorefChains(nlpFile)

    // add ner info to coref chains
    val nerMap =     // map of ner to mention
      (
        for((nerType , mention) <- ners) yield {
          (mention -> nerType)
        }
        ).toMap.withDefaultValue("O")


    val nerCorefChains =
      for(chain <- corefChains) yield {
        for(mention <- chain) yield {
          NlpData.xmlMention2NerMention(nerMap(mention), mention)
        }
      }


    // add singleton coref chains
    val mentionsInCoref = // set of mentions that should not be included in the singleton chains
      (
        for(chain <- corefChains; mention <- chain) yield mention
        ).toSet


    val nerSingletonCorefChains =
      for((nerType , mention) <- ners; if(!mentionsInCoref.contains(mention))) yield {
        Seq(NlpData.xmlMention2NerMention(nerType, mention))
      }

    nerCorefChains ++ nerSingletonCorefChains
  }



  def findCorefChainForText(text:String, corefChains:scala.Seq[scala.Seq[NlpXmlNerMention]], normalizeName: String => String ):scala.Seq[scala.Seq[NlpXmlNerMention]] = {
    val normName = normalizeName(text)
    val chainsWithName = corefChains.filter(chain =>  {
      chain.exists(mention => {
        normalizeName(mention.text) equals normName
      })
    })
    chainsWithName
  }



  def textStringDetector(docText:String, mention:String):Seq[NlpXmlNerMention]  = {
    val mentionTokens = mention.split(" ")
    val nerMentions = new ListBuffer[NlpXmlNerMention]

    var currIndex = docText.indexOf(mention)
    while(currIndex > -1){
      val charBegin = currIndex
      val charEnd = currIndex + mention.length()

      val tokenNumber = docText.substring(0, charBegin).replaceAll("\\s+", " ").count(_ == ' ')
      val tokenEnd = tokenNumber + mentionTokens.length

      var localCharBegin = charBegin
      val tokenList =
        for(tok <- mentionTokens) yield {
          val resultToken = Token(tok, "UNK", "?", tok, localCharBegin, localCharBegin + tok.length())
          localCharBegin += tok.length() + 1
          resultToken
        }
      nerMentions += new NlpXmlNerMention(mention, tokenList, 0, false, tokenNumber, tokenEnd, charBegin, charEnd, "UNK")

      currIndex = docText.indexOf(mention, currIndex+mention.length)
    }

    nerMentions
  }


  def allSentences(query: EntityMention): Seq[String] = {
// TODO: Create other implementations for the books / KBA!!!

//     if (query.isInstanceOf[BookELQueryImpl]) {
//         val bookQuery = query.asInstanceOf[BookELQueryImpl]
//         val sentences = bookQuery.contextTokens.groupBy(s => s.sentence).toList.sortBy(_._1)
//         val sentenceStrings = sentences.map(s => s._2.mkString(" "))
//         println(sentenceStrings.mkString(" "))
//         sentenceStrings.toSeq
//     } else if (query.isInstanceOf[KbaQueryNerMention]) {
//      val kbaSearcher = KnowledgeBaseSearcher.getSearcher("kba")
//      val r = kbaSearcher.getDocument(query.docId, true)
//      if (r != null) {
//        val nerData = r.metadata.get("nerData")
//        val sents = NerLineReader.extractSentencesFromNerData(nerData)
//        sents.map(_.text)
//      } else Seq()
//    } else {
      val nlpFile = NlpExtractor.getOrQueueNlp(() => {query.fullText}, query.docId, "tac")
      if (nlpFile.isDefined) {
        val sentences = TokenXmlReader.getSentences(nlpFile.get)
        sentences.map(sent => sent.tokens.map(_.word).mkString(" "))
      } else {
        PoorMansNlpExtractor.splitSentences(query.fullText.replace("\\s+", " "))
      }
  //  }
  }

//
//  def sentencesForQuery(fullText:()=>String,  docid:String, source:String): scala.Seq[String] = {
//    var nlpFile = NlpExtractor.getOrQueueNlp(fullText, docid, source)
//    if (nlpFile.isDefined){
//      val sentences = TokenXmlReader.getSentences(nlpFile.get)
//      sentences.map(sent => sent.tokens.map(_.word).mkString(" "))
//    } else {
//      PoorMansNlpExtractor.splitSentences(PoorMansNlpExtractor.cleanText(fullText()))
//    }
//  }



}
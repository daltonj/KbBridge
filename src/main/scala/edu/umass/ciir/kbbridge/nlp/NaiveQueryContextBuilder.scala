package edu.umass.ciir.kbbridge.nlp


import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import edu.umass.ciir.kbbridge.data.EntityMention
import edu.umass.ciir.models.StopWordList
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlMention
import edu.umass.ciir.kbbridge.util.NameDictionary

case class QueryContext(altNames: Seq[String], contextNers: Seq[String], allNersSorted:Seq[String], sentencesForMention:Seq[String]=Seq())



class NaiveQueryContextBuilder {





  def buildContext(query: EntityMention) : QueryContext = {

     var contexts = new HashSet[String]() 
    if (query.entityName.length > 0) {
    try {
//     var nlpFile = NlpExtractor.getOrQueueNlp(() => {mention.fullText}, mention.docId, mention.source)
     var neighbors = query.nerNeighbors.map(n => n.text).filterNot(ner => NameDictionary.DAY_MONTH_NAMES contains TextNormalizer.normalizeText(ner) ).filterNot(ner => ner.startsWith("http") || ner.length < 2)

      contexts = extractNameVariantsComponents(query.entityName, neighbors, true)
      //println("all coref: " + query.queryId + ": " + mention.name + ":" + mTexts.mkString("\t"))
     } catch {
       case _ => println("Error getting NLP data for mention " + query.toString())
    }
   
//    val fullTextSeq = Seq(query.fullText)
//    val fullTextVariants = extractNameVariantsComponents(query.entityName, fullTextSeq, true)

    //println("coref name variants: " + query.queryId + ": "+  mention.name + ":" + contexts.mkString("\t"))
    //println("text name variants: " + query.queryId + ": "+  mention.name + ":" + fullTextVariants.mkString("\t"))

  //  contexts ++= fullTextVariants
    }
    new QueryContext(contexts.toSeq, Seq(), Seq())
   
    
    
    
  }

  def getCorefChainForMention(query: EntityMention): Seq[NlpXmlMention] = {
    val normName =   TextNormalizer.normalizeText(query.entityName)
    //      println("name = "+name+" normname = "+normName)

    val corefChains = query.nerNeighbors
    //      val corefChains = NerExtractor.getXmlCorefForMentionFromFullText(() => {elQuery.fullText}, mention.docId, source = source)
    val chainsWithName = corefChains.filter(mention => {
        TextNormalizer.normalizeText(mention.text) equals normName

    })
    chainsWithName
  }
  
  def extractNameVariantsComponents(query: String, mentions:Seq[String], includeStopStructure: Boolean): HashSet[String] = {
    
    var contextTerms = new HashSet[String]
    var mentionsSeq = mentions
    for (coref <- mentionsSeq) {
      
      var cleanCoref = coref.replaceAll("[^A-Za-z01-9 ]", " ")
      var tokens = cleanCoref.split("\\s+").filter(s => s.length() > 1)
      
      var capTextSpans = new ListBuffer[String]
      var sequences = new ListBuffer[Seq[String]]()
      for (token <- tokens) {
        val normalToken = TextNormalizer.normalizeText(token)
        if (Character.isUpperCase(token.charAt(0))) {
          if ((!StopWordList.isStopWord(normalToken) || includeStopStructure)) {
               capTextSpans += token
          }
        } else {
          
          // add stopwords as part of sequences (acronyms, etc...)
          // we can remove later when querying.
          if (includeStopStructure && StopWordList.isStopWord(token)) {
             if ( capTextSpans.size > 0) {
                  capTextSpans += token
             }
          } else {
              // end a sequence and start over
            
              // prune stopwords at the end of sequence
              var pruned = prune( capTextSpans, includeStopStructure)
              if (pruned.length <= 10) {
                  sequences += pruned
              }
              
               capTextSpans = new ListBuffer[String]
          }
        }
      }
      sequences +=  capTextSpans
    
       // look through each capitalized term sequence and find possible acronyms 
      for (capSeq <- sequences) {
        var sb = new StringBuilder
        var numStopWords = 0
        for (tok <- capSeq) {
          
          // don't append capitalized stopwords.
          if(!StopWordList.isStopWord(tok)) {
              sb.append(tok.charAt(0))
          } else {
            numStopWords+=1
          }
        }
        
        val idx = sb.toString().toLowerCase().indexOf(TextNormalizer.normalizeText(query))
        if (idx > -1) {
            var numNonWords = 0
            var curIdx = idx
            val buf = new ListBuffer[String]
            while (numNonWords < query.length() && curIdx < capSeq.length) {
               
                var tok = capSeq(curIdx)
                if (!StopWordList.isStopWord(tok)) {
                  numNonWords+=1
                }
                curIdx+=1
                buf += tok
            }
            contextTerms += buf.mkString(" ")
        }
      }
      
      val normalizedQuery = TextNormalizer.normalizeText(query)
      val matchingSequences =
       if (normalizedQuery.length > 3) {
         sequences.filter(seq => {
           val normalizedString = TextNormalizer.normalizeText(seq.mkString(" "))
           (normalizedString contains normalizedQuery) && (!normalizedString.equals(normalizedQuery))  })
       } else {
         Seq()
       }

        val symbolsToNothing = query.toLowerCase().replaceAll("[^a-z01-9 ]", "")
        if (!(symbolsToNothing equals normalizedQuery)) {
          contextTerms += symbolsToNothing
        }

      // now we have all the matching capitalized sequences (and possible acronyms)
      var normalized = matchingSequences.map(seq => seq.mkString(" "))
      contextTerms ++= normalized
    }
    contextTerms
  }
  
  def isAllUpper(s: String) : Boolean = {
     
     for (i <- 0 to s.length()-1)
            if (!Character.isUpperCase(s.charAt(i)))
                return false;
        return true;
  }
  
  def prune(buffer: ListBuffer[String], stopStructure: Boolean): ListBuffer[String] =  {
    if (buffer.size > 0 && (!Character.isUpperCase(buffer.last.charAt(0)) || StopWordList.isStopWord(TextNormalizer.normalizeText(buffer.last)))) {
       buffer.remove(buffer.size-1)
       prune(buffer, stopStructure)
    }
    buffer
  }
}
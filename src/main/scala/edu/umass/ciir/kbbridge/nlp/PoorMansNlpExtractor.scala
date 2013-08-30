package edu.umass.ciir.kbbridge.nlp

import collection.mutable.ListBuffer
import edu.umass.ciir.kbbridge.nlp.NlpData.{Token, NlpXmlNerMention}
import edu.umass.ciir.models.StopWordList

/**
 *   //==================================
  //== Fall back for when NLP data is not available ==
  //--
  //-

 */
object PoorMansNlpExtractor {

  def textToTokenWithOffset(cleanText:String): ListBuffer[NlpData.Token] = {
    val tokens = new ListBuffer[Token]
    var idxText = cleanText.toCharArray.zipWithIndex
    while (idxText != null && !idxText.isEmpty) {
      val (chunk, rest) = idxText.span(_._1.isLetterOrDigit)
      idxText = rest.dropWhile(!_._1.isLetterOrDigit)

      if (chunk.length > 1) {
        val rawWord = chunk.map(_._1).mkString
        val beginOffset = chunk.head._2
        val endOffset = chunk.last._2 + 1
        tokens += Token(rawWord, "UNK", "?", rawWord, beginOffset, endOffset)
      }
    }
    tokens
  }


  def computeSentenceOffsets(cleanText:String):Seq[(Int,Int)] = {
    var idxText = cleanText.toCharArray.zipWithIndex


    // find sentence boundaries
    val sentOffsetToSentId0 = idxText.filter(_._1=='.').map(_._2).zipWithIndex
    val sentOffsetToSentId = sentOffsetToSentId0.map(offsetId => (offsetId._1, offsetId._2+1)) // sentence ids start with 1
    sentOffsetToSentId
  }
  def offsetToSentId(offset:Int, sentOffsetToSentId:Seq[(Int,Int)]):Int = {
    val lastOpt = sentOffsetToSentId.takeWhile(offsetId => offsetId._1 <= offset).lastOption     // "." included therefore <=
    lastOpt match {
      case Some((sentEndOffset, sentId)) => {
        if (offset > sentEndOffset) sentId + 1  // in case last sentence did not end on a "."
        else sentId
      }
      case None => 1 // actually there are no sentences
    }
  }


  def splitSentences(cleanText:String):Seq[String] = {
    cleanText.split("\\.").map(str => str.trim+".")
  }

  def sentenceContainsName(sent:String, names:Seq[String]):Boolean = {
    val s = TextNormalizer.normalizeText(sent)
    for (n <- names){
      if (s.contains(TextNormalizer.normalizeText(n))){
        true
      }
    }
    false
  }



  def poorMansNerSpanDetector(docText:String):Seq[Seq[NlpXmlNerMention]]  = {



    def identifyAllCapSpans(allTokens: ListBuffer[NlpData.Token]): ListBuffer[NlpXmlNerMention] = {
      val poorMansNerMentions = new ListBuffer[NlpXmlNerMention]


      def submitCapTextSpans(capTextSpans:ListBuffer[Token], tokenBeginIdx:Int) {
        if(!capTextSpans.isEmpty){
          // omit stopwords and uncapitalized words at the end of the sequence
          val chopOffIdx =
            capTextSpans.lastIndexWhere(tok => {
              tok.word.head.isUpper &&
                !StopWordList.isStopWord(TextNormalizer.normalizeText(tok.word))
            })
          if (chopOffIdx <= 5 && chopOffIdx > -1 ) {
            val capitalizedSpan = capTextSpans.take(chopOffIdx+1)
            val text = capitalizedSpan.map(_.word).mkString(" ")
            val tokenEnd = tokenBeginIdx + chopOffIdx + 1
            val mention = new NlpXmlNerMention(text, capTextSpans.toSeq, 0, false,tokenBeginIdx, tokenEnd, capitalizedSpan.head.charBegin, capitalizedSpan.last.charEnd, "UNK")
            poorMansNerMentions += mention
          }
        }
      }

      var capTextSpans = new ListBuffer[Token]
      var tokenBeginIdx = 0

      for ((tok,tokIdx) <- allTokens.zipWithIndex) {
        val token = tok.rawWord

        val normalToken = TextNormalizer.normalizeText(token)
        if (token.head.isUpper && !StopWordList.isStopWord(normalToken)) {
          if(capTextSpans.isEmpty) tokenBeginIdx = tokIdx
          capTextSpans += tok
        } else if (!capTextSpans.isEmpty && StopWordList.isStopWord(token)) {
          // add stopwords in the middle of sequences (acronyms, etc...)
          capTextSpans += tok
        } else {
          // end a sequence and start over
          if(!capTextSpans.isEmpty){
            submitCapTextSpans(capTextSpans, tokenBeginIdx)
            capTextSpans.clear()
          }
        }
      }
      submitCapTextSpans(capTextSpans, tokenBeginIdx)

      poorMansNerMentions
    }

    val cleantext = cleanText(docText)
    val tokens: ListBuffer[NlpData.Token] = textToTokenWithOffset(cleantext)
    val poorMansNerMentions = identifyAllCapSpans(tokens)

    //    // debug output
    //    for(mention <- poorMansNerMentions){
    //      println("poor man: "+mention)
    //    }


    poorMansNerMentions.toSeq.groupBy(_.text).values.toSeq
  }

  def cleanText(dirtyText: String): String = {
    dirtyText.replaceAll("[^A-Za-z01-9 ]", " ")
  }
}

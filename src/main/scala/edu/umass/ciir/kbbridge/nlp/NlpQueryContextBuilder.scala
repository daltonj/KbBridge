package edu.umass.ciir.kbbridge.nlp

import collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlMention
import edu.umass.ciir.models.StopWordList
import edu.umass.ciir.kbbridge.data.EntityMention

class NlpQueryContextBuilder {
  val normalizeText = TextNormalizer.normalizeText _

  def acronymText(name: String): String = {
    normalizeText(name).filter(_.isUpper)
  }

  val stopPosTags = Set("PRP", "IN", "TO", "PRP$")

  def acronymTexts(mention: NlpXmlMention): Set[String] = {
    val mainTokens =
      mention.tokens.filter(token => {
        !(StopWordList.isStopWord(token.word.toLowerCase)) ||
          (!(stopPosTags contains token.pos))
      })
    val acronym1 = mainTokens.map(_.word.head).mkString("")
    Set(acronymText(mention.text), acronym1)
  }

  //==================================
  //== Coref ==
  //--
  //-

  def corefChainOfMention(query: EntityMention): Seq[NlpXmlMention] = {

    val name = query.entityName

    val normName = normalizeText(name)
    val corefChains = NlpReader.getNerSpans(query)
    val chainsWithName = corefChains.filter(chain => {

      chain.exists(mention => {
        (normalizeText(mention.text) equals normName) || // in sync with...
          (acronymTexts(mention) contains normName)
      })

    })
    chainsWithName.flatten

  }

  def getAlternateNamesFromCoref(query: EntityMention, chain: Seq[NlpXmlMention]): Seq[String] = {
    val chains = for (m <- chain; if (m.tokens.exists(_.pos.startsWith("NNP")))) yield {

      // extract sequences of proper nouns
      var buffer = new ListBuffer[NlpData.Token]
      var sequences = new ListBuffer[String]()

      for (token <- m.tokens) {
        if (token.pos.startsWith("NNP")) {
          buffer += token
        } else {
          if (buffer.size > 0) {
            sequences += (for (tok <- buffer) yield tok.word).mkString(" ")
            buffer = new ListBuffer[NlpData.Token]
          }
        }
      }
      if (buffer.size > 0) {
        sequences += (for (tok <- buffer) yield tok.word).mkString(" ")
      }

      // filter the proper nouns to probable name matches to our original query
      val filteredSequences = for (sequence <- sequences.toList;

                                   if (normalizeText(sequence) contains normalizeText(query.entityName)) || // in sync with...
                                     (acronymText(sequence) equals normalizeText(query.entityName)))
      yield sequence

      filteredSequences.toSeq
    }
    chains.flatten
  }

  // warning, these offsets refer to the clean text, which might be modified from the galago full text
  def getPositionsFromCoref(chain: Seq[NlpXmlMention]): Seq[(Int, Int)] = {
    for (m <- chain) yield (m.charBegin, m.charEnd)
  }


  //==================================
  //== NER ==
  //--
  //-

  def allNers(query: EntityMention): scala.Seq[(String, NlpXmlMention)] = {
    val fullners = NlpReader.getNerSpans(query)
    for (chain <- fullners; mention <- chain; if (
      (mention.text.length > 2) &&
        !mention.text.toLowerCase.startsWith("http") &&
        mention.tokens.exists((tok => tok.pos.toLowerCase.startsWith("nn") || tok.pos == ("?"))) // ? is from poor mans ner
      )
    ) yield {
      (mention.ner, mention)
    }
    //
    //    var nlpFile = NlpExtractor.getOrQueueNlp(() => {
    //      query.fullText
    //    }, query.docId, query.source)
    //    val ners = NlpExtractReader.getNersDetails(nlpFile.get)
    //    ners
  }

  def nerSpansOfMention(normAltNames: Set[String], ners: Seq[(String, NlpXmlMention)]) = {

    val nersForQuery =
      for ((ner, mention) <- ners; if (normAltNames contains normalizeText(mention.text))) // keep in sync with...
      yield mention

    nersForQuery
  }

  //
  //  def relaxedNerMatches(query:TacELQuery2, ners:Seq[(String,  NlpXmlMention)]) = {
  //    val name = query.name
  //    for((ner, mention) <- ners) {
  //     val (left, right) = mention.text.split(name)
  //     if((left == "" || left.last = " ") && (right == "" || right.head = " ")){
  //       // substring match at word boundary
  //     }
  //
  //      for(tok <- mention.tokens) {
  //
  //      }
  //    }
  //  }


  //==================================
  //== Sentences ==
  //--
  //-

  def allSentences(query: EntityMention): scala.Seq[String] = {
    NlpReader.allSentences(query)
  }

  //  def allSentences(query: TacELQuery2): scala.Seq[String] = {
  //    if (ConfInfo.useKbaNlpReader) {
  //      val kbaSearcher = KnowledgeBaseSearcher.getSearcher("kba")
  //      val r = kbaSearcher.getDocument(docId, true)
  //      if (r != null) {
  //        val nerData = r.metadata.get("nerData")
  //        val sents = NerLineReader.extractSentencesFromNerData(nerData)
  //        sents.map(_.text)
  //      } else Seq()
  //    } else {
  //
  //      var nlpFile = NlpExtractor.getOrQueueNlp(() => {
  //        query.fullText
  //      }, query.docId, query.source)
  //      if (nlpFile.isDefined){
  //        val sentences = TokenXmlReader.getSentences(nlpFile.get)
  //        sentences.map(sent => sent.tokens.map(_.word).mkString(" "))
  //      } else {
  //        query.fullText.replace("\\s+", " ").split(".").toSeq.map(_.trim)
  //      }
  //    }
  //  }

  //==================================
  //== put everything together ==
  //--
  //-


  def buildContext(query: EntityMention): QueryContext = {

    val corefChains = corefChainOfMention(query)
    val altNamesFromCoref = getAlternateNamesFromCoref(query, corefChains)
    buildContextFromAltNames(query, altNamesFromCoref, corefChains)
  }

  def buildContextFromAltNames(query: EntityMention, altNamesFromCoref: Seq[String], corefChainsIfAvail: Seq[NlpXmlMention] = Seq()): QueryContext = {
    val ners = allNers(query)

    val corefChains =
      if (!corefChainsIfAvail.isEmpty) corefChainsIfAvail
      else {
        ners.map(_._2).filter(ner => {
          val normText = normalizeText(ner.text)
          altNamesFromCoref.exists(name => normalizeText(name) == normText)
        })
      }
    //    println(ners.mkString("$$$$ All Ners with NLP \n", "\n",""))
    //    println(ners.map(_._2.text).mkString("$$$$ All Ners ",", ", ""))

    val normalizedQuery = normalizeText(query.entityName)

    val normNamesSet = (altNamesFromCoref.map(name => normalizeText(name)) ++ Seq(normalizedQuery)).toSet
    val nersForMention = nerSpansOfMention(normNamesSet, ners)
    val sentenceSet = (
      nersForMention.map(_.sentenceId) ++
        corefChains.map(_.sentenceId)
      ).toSet
    val nersNotForMention: Seq[(String, NlpXmlMention)] = ners.filter(nm => !nersForMention.contains(nm._2))

    val nerContextTypes = Set("ORGANIZATION", "PERSON", "LOCATION", "UNK", "?")
    val nersInSentence =
      for ((ner, mention) <- nersNotForMention; if (sentenceSet.contains(mention.sentenceId) && (nerContextTypes contains ner)))
      yield mention

    val sentencesForMention = {
      val sents = allSentences(query)
      if (sentenceSet contains 0) {
        sents.filter(sent => normNamesSet.exists(nameVar => sent.contains(nameVar)))
      } else {
        sentenceSet.toSeq.sorted.map(idx => sents.get(idx - 1))
      }

    }


    val queryOffsets = nersForMention.flatMap(ner => Seq(ner.charBegin, ner.charEnd))

    val nersByOffsetDistance =
      if (queryOffsets.isEmpty) {
        //System.err.println("TacNlpQUeryContextBuilder: No queryOffsets found for "+query+". Not returning Ners.")
        nersNotForMention.map(_._2.text)
      }
      else {
        val sortedNersWithOffset: Seq[(String, Int)] = nersNotForMention.map(nm => {
          val mention = nm._2
          val distance = computeDistanceToOffsets(mention, queryOffsets)
          (nm._2.text, distance)
        }).sortBy(_._2)
        //       println("ners with distance = "+ sortedNersWithOffset.mkString(", "))
        val nersByOffsetDistance = sortedNersWithOffset.map(_._1.toLowerCase).distinct
        nersByOffsetDistance
      }

    //    println("dedup ners = "+nersByOffsetDistance)

    //    var fullTextSeq = new ListBuffer[String]
    //    fullTextSeq += query.fullText


    val names = ((normNamesSet ++ (nersForMention.map(ner => normalizeText(ner.text)))) - normalizedQuery).toSeq

    //    println("all coref: " + query.queryId + ": " + query.name + ":" + corefChains.mkString("\t"))
    //    println("coref alt: " + query.queryId + ": " + query.name + ":" + altNamesFromCoref.mkString("\t"))
    //    println("NERs: " + query.queryId + ": "+  query.name + ":" + nersForMention.mkString("\t"))
    //    println("contextual NERs: " + query.queryId + ": "+  query.name + ":" + nersInSentence.mkString("\t"))
    //    println("altNames " + query.queryId + ": "+  query.name + ":" + names.mkString("\t"))
    QueryContext(altNames = names, contextNers = nersInSentence.map(_.text), allNersSorted = nersByOffsetDistance, sentencesForMention)
  }


  def computeDistanceToOffsets(mention: NlpData.NlpXmlMention, queryOffsets: Seq[Int]): Int = {
    val cb = mention.charBegin
    val ce = mention.charEnd
    val distance = queryOffsets.map(offset => math.min(math.abs(cb - offset), math.abs(ce - offset))).min
    distance
  }
}
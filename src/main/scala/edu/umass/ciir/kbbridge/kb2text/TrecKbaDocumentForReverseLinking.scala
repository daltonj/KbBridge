package edu.umass.ciir.kbbridge.kb2text

import edu.umass.ciir.treckba.NerLineReader
import edu.umass.ciir.kbbridge.nlp.TextNormalizer
import edu.umass.ciir.kbbridge.features.util.SetMeasures
import org.lemurproject.galago.core.parse.Document
import edu.umass.ciir.kbbridge.data.{SimpleEntityMention, EntityMention, BridgeDocument}
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention
import edu.umass.ciir.kbbridge.nlp.TokenXmlReader.Sentence
import collection.mutable.ListBuffer
import edu.umass.ciir.kbbridge.data.repr.EntityRepr
import edu.umass.ciir.kbbridge.text2kb.TextEntityReprGeneratorsUtil

/**
 * User: dietz
 * Date: 6/11/13
 * Time: 5:38 PM
 */
class TrecKbaDocumentForReverseLinking(galagoDoc:BridgeDocument, originalQueryName:String, allNames:Iterable[String], val originalQueryEntityRepr:Option[EntityRepr], val score:Option[Double]=None, val rank:Option[Int]=None) {

  val (ners, sentences) = {
    val nerData = galagoDoc.metadata.get("nerData").getOrElse("")
    //println(nerData)
    val ners = NerLineReader.extractNerMentions(nerData).map(pair => pair._2)
    val sentences = NerLineReader.extractSentencesFromNerData(nerData)
    (ners, sentences)
  }

  val (filteredNames, filteredNers) = findMatches(allNames.toSeq, ners, sentences)


//  println("Q:" + originalQuery.getRawQuery() + " " + kbaResult.rank + " " + galagoDoc.identifier + " mention: "  + representativeMentionInDoc)
  val nerType = "PER"
  val docId = galagoDoc.documentname

  // take the wikipedia results as document comentions
  def nerMentionQuery = {
    val fulltext = sentences.map(_.text).mkString(" \n")
    SimpleEntityMention(docId = docId, entityType = nerType, entityName = originalQueryName, corefChain = filteredNers, fullText = fulltext, mentionId = docId)
  }

  val nerMentionEntityRepr = {
    val fulltext = sentences.map(_.text).mkString(" ")
    val weightedNames = TextEntityReprGeneratorsUtil.uniformWeighting(filteredNames)
    val weightedWords =TextEntityReprGeneratorsUtil.createWeightedWords(fulltext)
    EntityRepr(entityName = originalQueryName, nameVariants = weightedNames, words = weightedWords)
  }

    //new KbaQueryNerMention(kbaResult.documentName, nerType, originalQuery + "_" + kbaResult.documentName, representativeMentionInDoc, "", false, None, filteredNers)


  def exactMatches(nameVariants: Seq[String], ners : Seq[NlpXmlNerMention], sentences: Seq[Sentence]) : (Seq[String], Seq[NlpXmlNerMention]) = {
    val normalizedNames = nameVariants.map(name => TextNormalizer.normalizeText(name)).toSet
    val coreferentNers = ners.filter(mention =>  (normalizedNames contains TextNormalizer.normalizeText(mention.text)) )


    if (coreferentNers.size == 0) {
      val textMatches = new ListBuffer[String]
      // match free text
      val text =
        sentences.map(s => TextNormalizer.normalizeText(s.text)).mkString(" ")
      for (name <- normalizedNames) {
        if (text contains name) {
          textMatches += name
        }
      }
      (textMatches.toSeq, Seq())
    } else {
      (coreferentNers.map(ner => ner.text), coreferentNers)
    }

  }


  def findMatches(allNames:Seq[String], ners:Seq[NlpXmlNerMention], sentences:Seq[Sentence]) : (Seq[String], Seq[NlpXmlNerMention]) = {

    var (filteredNames, filteredNers) = exactMatches(allNames, ners, sentences)

    if (filteredNames.size == 0 ) {
      // back-off to subsequence matches?
      filteredNames = tokenMatches(allNames, ners, sentences)._1
    }

    var representativeMentionInDoc = ""
    if (filteredNames.size > 0) {

      // we have matches, go through and prioritize them to pick the best one
      val normalized = filteredNames.map(name => TextNormalizer.normalizeText(name)).toSet
      representativeMentionInDoc = {
        if (normalized contains TextNormalizer.normalizeText(originalQueryName)) {
          originalQueryName
        } else {
          //           filteredNames.maxBy(name => name.size)
          // sort the ners based on the token dice score
          val allNameTokens = allNames.map(name => TextNormalizer.normalizeText(name).split("\\s+")).flatten.toSet
          val scoredNames = filteredNames.map(mention => {
            val mentionTokenSet = TextNormalizer.normalizeText(mention).split(" ").toSet
            val similarity = SetMeasures(allNameTokens, mentionTokenSet)
            (similarity.intersect, mention)
          })

          val sorted = scoredNames.toList.sortBy(r => (-r._1,-r._2.size))

          sorted.head._2
        }
      }
    }

    (filteredNames, filteredNers)
  }

  def tokenMatches(nameVariants: Seq[String], ners : Seq[NlpXmlNerMention], sentences: Seq[Sentence]) : (Seq[String], Seq[NlpXmlNerMention]) = {
    val tokens = nameVariants.map(name => TextNormalizer.normalizeText(name).split("\\s+")).flatten.toSet
    val coreferentNers = ners.filter(mention =>  ((TextNormalizer.normalizeText(mention.text).split(" ").toSet intersect tokens).size > 0 ))
    (coreferentNers.map(ner => ner.text),coreferentNers)
  }


  def normalizeSentence(sent: Sentence): String = {
    TextNormalizer.normalizeText(sent.tokens.map(_.word).mkString(" "))
  }

  def sentenceOfQuery(q: EntityMention, nameVariances: Seq[String])(normSentence: String): Boolean = {
    val names: Seq[String] = nameVariances :+ q.entityName
    names.exists(name => normSentence.contains(TextNormalizer.normalizeText(name)))
  }


}

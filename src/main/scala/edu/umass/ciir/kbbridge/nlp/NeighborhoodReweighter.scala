package edu.umass.ciir.kbbridge.nlp

import collection.mutable.ListBuffer
import edu.umass.ciir.memindex.Query
import scala.collection.JavaConversions._
import org.lemurproject.galago.core.retrieval.ScoredDocument
import edu.umass.ciir.models._
import edu.umass.ciir.kbbridge.data.{SimpleEntityMention, EntityMention}
import edu.umass.ciir.kbbridge.util.ConfInfo
import edu.umass.ciir.kbbridge.search.{DocumentBridgeMap, GalagoRetrieval}


/**
 * Created with IntelliJ IDEA.
 * User: dietz
 * Date: 8/29/12
 * Time: 8:50 PM
 * To change this template use File | Settings | File Templates.
 */
class NeighborhoodReweighter {
  val normalizeText = TextNormalizer.normalizeText _

  def acronymText(name: String): String = {
    normalizeText(name).filter(_.isUpper)
  }

  // =============
  // match sentences in source doc

  //  def allSentences(query: EntityMention): scala.Seq[String] = {

  /**
  Does not give sentences when nlp file is not existing
    */
  //  def allSentences(query: EntityMention): scala.Seq[Sentence] = {
  //    //    NlpReader.allSentences(() => {
  //    //      query.fullText
  //    //    }, query.docId, query.source)
  //
  //
  //    var nlpFile = NlpExtractor.getOrQueueNlp(() => {
  //      query.fullText
  //    }, query.docId, query.source)
  //    if (nlpFile.isEmpty) {
  //      Seq()
  //    } else {
  //      val sentences = TokenXmlReader.getSentences(nlpFile.get)
  //      sentences
  //    }
  //  }


  def sentenceOfQuery(q: EntityMention, nameVariances: Seq[String])(normSentence: String): Boolean = {
    val names: Seq[String] = nameVariances :+ q.entityName
    names.exists(name => normSentence.contains(TextNormalizer.normalizeText(name)))
  }


  def normalizeSentence(sent: TokenXmlReader.Sentence): String = {
    TextNormalizer.normalizeText(sent.tokens.map(_.word).mkString(" "))
  }

  def normalizeSentence(sent: String): String = {
    TextNormalizer.normalizeText(sent)
  }

  // =================
  // reweight ners

  def buildTrivialReweights(query: EntityMention, nercontext: Seq[String], nameVariances: Seq[String] = Seq()
                           ): Map[String, Double] = {
    nercontext.map(ner => (TextNormalizer.normalizeText(ner) -> 1.0)).toMap
  }

  def buildLocalWeights(query: EntityMention, nercontext: Seq[String], nameVariances: Seq[String] = Seq()
                       ): Map[String, Double] = {

    val normalizedMentionNers = nercontext.map(name => TextNormalizer.normalizeText(name))
    val mentionEntityLm = new LanguageModel(1)
    mentionEntityLm.addDocument(normalizedMentionNers, false)
    mentionEntityLm.calculateProbabilities()
    val entries = mentionEntityLm.getEntries
    val termMap = entries.map(te => te.getTerm -> te.getProbability).toMap
    termMap
  }


  def reweightNersWithPseudorel(query: EntityMention, nercontext: Seq[String], nameVariances: Seq[String],
                                selectedNormSentences: Seq[String]
                               ): Map[String, Double] = {

    throw new Exception("Update me to use the new entity representation!")
    //    // fire query
    //    val pseudoresults = pseudoKbSearcher.search(new Query(query.mentionId + "-pseudo", query.entityName, ConfInfo.maxCandidatesPseudo), nameVariances.toList, nercontext.distinct, selectedNormSentences)
    //
    //    if (pseudoresults.length > 0) {
    //
    //      // get results as TacELQueries
    //      val pseudoQueries_score = new ListBuffer[(EntityMention, Double)]()
    //      val scoredDocs = new ListBuffer[ScoredDocument]
    //      for (r <- pseudoresults; if r.identifier != query.docId) {
    //        val pq = new SimpleEntityMention(docId = r.identifier, entityType = "UNK", mentionId = r.identifier, entityName = query.entityName, fullText="")
    //        println("PSEUDORESULT\t" + query.mentionId + "\t" + r.identifier + "\t" + r.score)
    //        scoredDocs += new ScoredDocument(r.identifier, r.rank, r.score)
    //        pseudoQueries_score += Pair(pq, r.score)
    //      }
    //
    //      val scores = RelevanceModel.logsToPosteriors2(scoredDocs.toList)
    //
    //      val nerDocFreq = scala.collection.mutable.HashMap[String, Int]()
    //      for (ner <- nercontext) nerDocFreq += (ner -> 0)
    //      val nerReweights = scala.collection.mutable.HashMap[String, Double]()
    //      for (ner <- nercontext) nerReweights += (ner -> 0.0)
    //
    //      println("scanning " + pseudoQueries_score.length + " pseudo results")
    //
    //      for ((pq, score) <- pseudoQueries_score) {
    //        val normtext = TextNormalizer.normalizeText(pq.fullText)
    //        val normTokens = normtext.split("\\s+")
    //        val docLm = new LanguageModel(4)
    //        docLm.addDocument(normTokens, true)
    //        docLm.calculateProbabilities()
    //
    //        for (ner <- nercontext) {
    //          val termEntry = docLm.getTermEntry(TextNormalizer.normalizeText(ner))
    //          if (termEntry != null) {
    //            val oldCount = nerDocFreq(ner)
    //            nerDocFreq.update(ner, oldCount + 1)
    //            val oldWeight = nerReweights(ner)
    //            val curWeight = termEntry.getProbability * scores(pq.docId)
    //            nerReweights.update(ner, oldWeight + curWeight)
    //          }
    //        }
    //      }
    //      val filteredWeights = nerReweights.filter(_._2 > 0.0).toMap
    //      filteredWeights
    //    } else {
    //      Map()
    //    }


    //    nercontextExtended.map(ner => (ner -> 1.0)).toMap
  }

  val nlpQueryBuilder = new NlpQueryContextBuilder()

  def reweightAddNersWithPseudorel(query: EntityMention, nercontext: Seq[String], nameVariances: Seq[String],
                                   selectedNormSentences: Seq[String], addPseudoNers: Boolean = false
                                  ): Map[String, Double] = {
    throw new Exception("Update me to use the new entity representation!")

    //    // fire query
    //    val pseudoresults = pseudoKbSearcher.search(new Query(query.mentionId + "-pseudo", query.entityName, ConfInfo.maxCandidatesPseudo), nameVariances.toList, java.util.Collections.emptyList(), selectedNormSentences)
    //    val scoredDocs = pseudoresults.map(r => new ScoredDocument(r.identifier, r.rank, r.score))
    //    val results = reweightDocsAddNew(scoredDocs, query, nercontext, nameVariances, selectedNormSentences, addPseudoNers)
    //    results
  }

  def reweightDocsAddNew(pseudoresults: Seq[ScoredDocument], query: EntityMention, nercontext: Seq[String],
                         nameVariances: Seq[String], selectedNormSentences: Seq[String], addPseudoNers: Boolean = false
                        ): Map[String, Double] = {
    if (pseudoresults.length > 0) {
      reweightDocsAddNewWithDocs(query, pseudoresults, nercontext, nameVariances, addPseudoNers)
    } else {
      Map()
    }
  }

  /**
   *
   * @param nercontext  first pass ner context. May be modified if addPseudoNers is true
   * @param nameVariances  if empty, the coref chains will be scanned.
   * @param addPseudoNers if true, new ners will be added/swapped in the nercontext
   * @return
   */
  def reweightDocsAddNewWithDocs(query: EntityMention, pseudoresults: Seq[ScoredDocument], nercontext: Seq[String],
                                 nameVariances: Seq[String] = Seq(), addPseudoNers: Boolean = false
                                ): Map[String, Double] = {

    if (pseudoresults.length > 0) {

      val scores = RelevanceModel.logsToPosteriors2(pseudoresults.toList)

      // get results as TacELQueries -- Why!?! --> just so we can reuse our NLP doc processing. This need to be fixed!
      val pseudoQueries_score = new ListBuffer[(EntityMention, Double)]()
      for (r <- pseudoresults; if r.documentName != query.docId) {
        val pq = new SimpleEntityMention(docId = r.documentName, entityType = "UNK", mentionId = r.documentName,
                                         entityName = query.entityName, fullText = "")
        println("PSEUDORESULT\t" + query.mentionId + "\t" + r.documentName + "\t" + r.score)
        pseudoQueries_score += Pair(pq, r.score)
      }

      val nerContextExtended =
        if (!addPseudoNers) {
          nercontext
        } else {
          // add new ners

          val nersForPseudos =
            for ((pq, _) <- pseudoQueries_score) yield {
              val pseudoContext =
                if (nameVariances.isEmpty) {
                  nlpQueryBuilder.buildContext(pq)
                } else {
                  nlpQueryBuilder.buildContextFromAltNames(pq, nameVariances, Seq())
                }
              // no k-closest restriction... could add this back here.
              val ners = pseudoContext.allNersSorted
              ners
            }

          (nercontext ++ nersForPseudos.flatten).distinct
        }

      val nerDocFreq = scala.collection.mutable.HashMap[String, Int]()
      for (ner <- nerContextExtended) nerDocFreq += (ner -> 0)
      val nerReweights = scala.collection.mutable.HashMap[String, Double]()
      for (ner <- nerContextExtended) nerReweights += (ner -> 0.0)

      println("scanning " + pseudoQueries_score.length + " pseudo docs")

      for ((pq, score) <- pseudoQueries_score) {
        val normtext = TextNormalizer.normalizeText(pq.fullText)

        val normTokens = normtext.split("\\s+")
        val docLm = new LanguageModel(4)
        docLm.addDocument(normTokens, true)
        docLm.calculateProbabilities()

        for (ner <- nerContextExtended) {
          val termEntry = docLm.getTermEntry(ner)
          if (termEntry != null) {
            val oldCount = nerDocFreq(ner)
            nerDocFreq.update(ner, oldCount + 1)
            val oldWeight = nerReweights(ner)
            nerReweights.update(ner, oldWeight + (termEntry.getProbability * scores(pq.docId)))
          }
        }
      }

      val topKNers =
        if (!addPseudoNers) nerReweights
        else {
          nerReweights.toSeq.sortBy(-_._2).take(10)
        }

      topKNers.filter(_._2 > 0.0).toMap
    } else {
      Map()
    }


    //    nercontextExtended.map(ner => (ner -> 1.0)).toMap
  }

  // ========
  // filter by sentence boundaries.
  // todo this should be done in the TacNlpQueryContext builder or the NaiveQUeryCOntextBUilder


  def pseudoKbSearcher: GalagoRetrieval = {
    new GalagoRetrieval(jsonConfigFile = ConfInfo.galagoDefaultJsonParameterFile,
                        galagoUseLocalIndex = ConfInfo.galagoUseLocalIndex, galagoSrv = ConfInfo.galagoKbSrv)
  }

  //  def getNormSentences(query: EntityMention): Seq[String] = {
  //    val nlpSentences: Seq[String] = NlpReader.allSentences(query)
  //
  //    val normSentences =
  //      if (!nlpSentences.isEmpty) {
  //        nlpSentences.map(TextNormalizer.normalizeText(_))
  //      } else {
  //        PoorMansNlpExtractor.splitSentences(query.fullText).map(sent => TextNormalizer.normalizeText(sent))
  //      }
  //    normSentences
  //  }

  def sentenceFilterNers(query: EntityMention, nercontext: Seq[String], nameVariances: Seq[String],
                         normSentences: Seq[String]
                        ): Seq[String] = {

    val queryFilter = sentenceOfQuery(query, nameVariances) _

    val badNers = ListBuffer[String]()
    badNers ++= nercontext

    val goodNers = ListBuffer[String]()

    for (normSent <- normSentences; if (!badNers.isEmpty && queryFilter(normSent))) {
      println("## searching sentence: " + normSent)
      for (ner <- badNers.toSeq) {
        if (normSent.contains(TextNormalizer.normalizeText(ner))) {
          badNers -= ner
          goodNers += ner
          println("retaining ner: " + ner)
        }
      }
    }


    println("dismissed ners: " + badNers.mkString(", "))
    println("retained ners: " + goodNers.mkString(", "))

    goodNers
  }


}

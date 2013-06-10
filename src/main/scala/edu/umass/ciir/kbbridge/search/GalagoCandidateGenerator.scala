package edu.umass.ciir.kbbridge.search


import org.lemurproject.galago.core.tools.Search
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import java.io._
import scala.collection.mutable.HashMap
import java.{lang, util}
import edu.umass.ciir.kbbridge.util.ConfInfo
import edu.umass.ciir.kbbridge.data._
import scala.Some
import edu.umass.ciir.kbbridge.data.ScoredWikipediaEntity
import edu.umass.ciir.kbbridge.data.WikipediaEntity
import edu.umass.ciir.kbbridge.data.EntityMention
import edu.umass.ciir.kbbridge.nlp.{NlpQueryContextBuilder, PseudoRelevanceNerReweighter, NaiveQueryContextBuilder, TextNormalizer}
import edu.umass.ciir.memindex.Query
import edu.umass.ciir.models.{StopWordList, LanguageModel}


class GalagoCandidateGenerator(candidateFileKey: String = ConfInfo.candidateFileKey,
                               useRunFile: Boolean = ConfInfo.candsFromRunFile,
                               useOracle: Boolean = ConfInfo.useOracleCandidateGeneration,
                               complainOnMissingTacId: Boolean = true,
                               galagoJsonParameterFile: String = ConfInfo.galagoJsonParameterFile) {

  val defaultNumCandidates = 100
  val galagoSearcherTacKb = KnowledgeBaseSearcher.getSearcher()

  val runFileName = ConfInfo.galagoRunDir + "/" + candidateFileKey + ".run"


  var booleanRunFileExists = false
  var writer: Option[java.io.PrintWriter] = None
  var candsFromRun: Option[HashMap[String, ListBuffer[WikipediaEntity]]] = None


  def getOracle(query: EntityMention): Option[Search.SearchResultItem] = {
    if (query.isInstanceOf[TacEntityMention]) getTacOracle(query.asInstanceOf[TacEntityMention])
    else {
      throw new Error("Don't know how to resolve oracle for query instance " + query.getClass.getName)
    }
  }

  def getRunWriter(): Option[PrintWriter] = {

    if (useRunFile) {
      if (new File(runFileName).exists()) {
        booleanRunFileExists = true
        println("Run file already exists. Not creating writer.")
      } else {
        booleanRunFileExists = false
        val dir = new File(ConfInfo.galagoRunDir)
        dir.mkdirs()
        return Some(new java.io.PrintWriter(runFileName, "UTF-8"))
      }
    } else {
      return None
    }

    None
  }


  def galagoResultToEntity(r: Search.SearchResultItem, score: Double, galagoSearcher: KnowledgeBaseSearcher, source: String): ScoredWikipediaEntity = {
    val wikititle = r.identifier
    val metadata: Map[String, String] = if (r.metadata != null) r.metadata.toMap else Map.empty
    var wikiId = -1
    if (metadata.size > 0) {
      wikiId = r.metadata.get("wikiId").toInt
    } else {
      wikiId = IdMap.wikiTitle2WikiIdMap(wikititle)
    }

    val tacIdListOpt = IdMap.wikiTitle2TacIdMap.get(wikititle)


    tacIdListOpt match {
      case Some(tacIdList) if (tacIdList != "") => {
        // this wiki entity is contained in tac kb
        new ScoredWikipediaEntity(
          wikititle
          , wikipediaId = wikiId
          , metadata = metadata
          , score = score
          , rank = r.rank
        )

      }
      case _ => {
        // this wiki entity is NOT contained in tac kb (i.e. "fullwiki")
        new ScoredWikipediaEntity(
          wikititle
          , wikipediaId = wikiId
          , metadata = metadata
          , score = score
          , rank = r.rank
        )
      }
    }
  }

  def findCandidateEntities(query: EntityMention, numCands: Int): Seq[ScoredWikipediaEntity] = {

    val cands =
      if (ConfInfo.useNerContextInQuery) {
        findCandidatesUsingCorefAndNerContext(galagoSearcherTacKb, query, numCands, useOracle, "wiki")
      } else {
        findCandidatesUsingCorefContext(galagoSearcherTacKb, query, numCands, useOracle, "wiki")
      }

    if (writer != None) {
      for ((result, idx) <- cands.zipWithIndex) {
        writer.get.println(query.mentionId + " " + result.wikipediaId + " "
          + result.wikipediaTitle + " " + (idx + 1) + " " + result.score)
      }
    }
    cands
    //     }
  }

  def findCandidatesUsingCorefContext(galagoSearcher: KnowledgeBaseSearcher, query: EntityMention, numCands: Int, useOracle: Boolean, source: String): Seq[ScoredWikipediaEntity] = {

    var contextBuilder = new NaiveQueryContextBuilder
    var context = contextBuilder.buildContext(query)

    var results = galagoSearcher.search(new Query(query.mentionId, query.entityName, numCands), context.altNames.toList, java.util.Collections.emptyList()).toList


    val candidateWikiEntries =
      results.toSeq.map(r => {
        galagoResultToEntity(r, r.score, galagoSearcher, source)
      })

    addOracleCandidates(useOracle, query, candidateWikiEntries, galagoSearcher, source, numCands)
  }

  def findCandidatesUsingCorefAndNerContext(galagoSearcher: KnowledgeBaseSearcher, query: EntityMention, numCands: Int, useOracle: Boolean, source: String): Seq[ScoredWikipediaEntity] = {

    val corefContextBuilder = new NaiveQueryContextBuilder
    val corefContext = corefContextBuilder.buildContext(query)


    val nerContextBuilder = new NlpQueryContextBuilder
    val nerContext = nerContextBuilder.buildContext(query)
    val nerReweighter = new PseudoRelevanceNerReweighter()


    val normSentences = nerReweighter.getNormSentences(query)

    val setOfContextNers = ConfInfo.nerNeighborQuerySelectMethod match {
      case "all" => {
        println("all: Using all ners from the query document")
        nerContext.allNersSorted
      }
      case "sentence" => {
        println("sentence: Using ners mentioned in the sentences mentioning the query")
        nerReweighter.sentenceFilterNers(query, nerContext.allNersSorted, nerContext.altNames, normSentences)
        //        nerContext.contextNers
        //nerReweighter.buildSentenceFilterReweights(tacQuery, nerContext.contextNers,corefContext.altNames)
      }
      case "kclosest" => {
        println("kclosest: Using k closest ners")
        nerContext.allNersSorted.take(ConfInfo.nerNeighborQueryK)
      }

    }


    val normQuery = TextNormalizer.normalizeText(query.entityName)
    val selectedSentences = normSentences.filter(sent => sent.contains(normQuery)).take(5).map(sent => {
      if (sent.length > 200)
        sent.substring(0, math.min(sent.length, math.max(200, sent.indexOf(" ", 200))))
      else sent
    })

    println("enriching query with sentences:\n" + selectedSentences.mkString("\n"))


    val normtext = TextNormalizer.normalizeText(query.fullText)
    val normTokens = normtext.split("\\s+")
    val unigramModel = new LanguageModel(1)
    unigramModel.addDocument(normTokens, true)
    unigramModel.calculateProbabilities()

    val unigramWeights = unigramModel.getKTopFrequencies(20, true, 1)


    val nerWeights =
      ConfInfo.nerNeighborQueryMethod match {
        case "uniform" => {
          nerReweighter.buildTrivialReweights(query, setOfContextNers, corefContext.altNames)
        }
        case "local" => {
          nerReweighter.buildLocalWeights(query, setOfContextNers, corefContext.altNames)
        }
        case "discount" => {
          nerReweighter.reweightNersWithPseudorel(query, setOfContextNers, corefContext.altNames, selectedSentences)
        }
        case "discountadd" => {
          nerReweighter.reweightAddNersWithPseudorel(query, setOfContextNers, corefContext.altNames, selectedSentences, addPseudoNers = true)
        }
        case _ => throw new Error("Option for ConfInfo.nerNeighborQueryMethod  not supported")
      }

    val uniformNer = nerReweighter.buildTrivialReweights(query, setOfContextNers, corefContext.altNames)
    val localNer = nerReweighter.buildLocalWeights(query, setOfContextNers, corefContext.altNames)
   // val rmReweightedNer = nerReweighter.reweightNersWithPseudorel(query, setOfContextNers, corefContext.altNames, selectedSentences)
   // val rmNer = nerReweighter.reweightAddNersWithPseudorel(query, setOfContextNers, corefContext.altNames, selectedSentences, addPseudoNers = true)

    val nerWeightNoStop = nerWeights.filter(entry => {
      val string = entry._1
      stringShortOrOnlyStopwords(string)
    })

    val useSelSentences =
      if (ConfInfo.useSentencesInCandidateQuery) {
        selectedSentences
      } else Seq()

    println("running first pass with context... (v 0.1)")
    val firstPassResults =
      if (!ConfInfo.noFirstPassQuery) {
        val firstPassResults = galagoSearcher.search(new Query(query.mentionId, query.entityName, numCands), corefContext.altNames.toList, java.util.Collections.emptyList(), Seq()).toList
        firstPassResults
      } else List()
    val results = firstPassResults

  //    galagoSearcher.searchComponents(new Query(query.mentionId, query.entityName, numCands),
  //    corefContext.altNames.toList, toJavaNeighborWeights(nerWeights), firstPassResults, useSelSentences,
  //    toJavaNeighborWeights(uniformNer), toJavaNeighborWeights(localNer), toJavaNeighborWeights(rmReweightedNer),
  //    toJavaNeighborWeights(rmNer)).toList


    val candidateWikiEntries =
      results.toSeq.map(r => {
        println("CANDRank:\t" + r.identifier + "\t" + r.rank + "\t" + r.score)
        galagoResultToEntity(r, r.score, galagoSearcher, source)
      })

    addOracleCandidates(useOracle, query, candidateWikiEntries, galagoSearcher, source, numCands)
  }

  def toJavaNeighborWeights(map: Map[String, Double]): util.Map[String, lang.Double] = {
    map.filter(!_._2.isNaN).map(entry => (entry._1, new java.lang.Double(entry._2))).toMap[String, lang.Double]
  }


  def stringShortOrOnlyStopwords(string: String): Boolean = {
    string.trim().length > 2 &&
      !string.split("\\s+").forall(token => StopWordList.isStopWord(token.toLowerCase))
  }

  def addOracleCandidates(useOracle: Boolean, query: EntityMention, candidateWikiEntries: Seq[ScoredWikipediaEntity], galagoSearcher: KnowledgeBaseSearcher, source: String, numCands: Int): Seq[ScoredWikipediaEntity] = {
    val oracle = if (useOracle) getOracle(query) else None

    val candidateAndOracleEntries =
      oracle match {
        case Some(r) => {
          if (!candidateWikiEntries.exists(_.wikipediaTitle == r.identifier)) {
            // oracle not in the list. add it with a min score.
            val minScore =
              if (candidateWikiEntries.isEmpty) {
                -1.0
              }
              else {
                candidateWikiEntries.map(_.score).min
              }
            val oracleEntity = galagoResultToEntity(r, minScore, galagoSearcher, source)
            println("oracle missing for " + query + " adding with score " + minScore + " " + oracleEntity)
            candidateWikiEntries.take(numCands - 1) :+ oracleEntity
          } else {
            candidateWikiEntries
          }
        }
        case None => {
          candidateWikiEntries
        }
      }

    candidateAndOracleEntries.map(_.asInstanceOf[ScoredWikipediaEntity]).toSeq
  }

  def findCandidateEntitiesFromSingleSearcher(galagoSearcher: KnowledgeBaseSearcher, query: EntityMention, numCands: Int, useOracle: Boolean, source: String): Seq[ScoredWikipediaEntity] = {
    var results = galagoSearcher.search(new Query(query.mentionId, query.entityName, numCands)).toList

    val oracle = if (useOracle) getOracle(query) else None

    val candidateWikiEntries =
      results.toSeq.map(r => {
        galagoResultToEntity(r, r.score, galagoSearcher, source)
      })

    val candidateAndOracleEntries =
      oracle match {
        case Some(r) => {
          if (!candidateWikiEntries.exists(_.wikipediaTitle == r.identifier)) {
            // oracle not in the list. add it with a min score.
            val minScore =
              if (candidateWikiEntries.isEmpty) {
                -1.0
              }
              else {
                candidateWikiEntries.map(_.score).min
              }
            val oracleEntity = galagoResultToEntity(r, minScore, galagoSearcher, source)
            println("orcle missing for " + query + " adding with score " + minScore + " " + oracleEntity)
            candidateWikiEntries.take(numCands - 1) :+ oracleEntity
          } else {
            candidateWikiEntries
          }
        }
        case None => {
          candidateWikiEntries
        }
      }

    candidateAndOracleEntries.map(_.asInstanceOf[ScoredWikipediaEntity]).toSeq
  }

  def getTacOracleAsWikiResult(query: EntityMention): Option[ScoredWikipediaEntity] = {
    getTacOracle(query) match {
      case Some(result) => {
        val wikiResult = galagoResultToEntity(result, result.score, galagoSearcherTacKb, "oracle")
        Some(wikiResult.asInstanceOf[ScoredWikipediaEntity])
      }

      case None => None
    }

  }

  def getTacOracle(query: EntityMention): Option[Search.SearchResultItem] = {
    val tacQuery = query.asInstanceOf[TacEntityMention]
    tacQuery.groundTruthWikiTitle match {
      case Some(wikiTitle) => {

        val result = galagoSearcherTacKb.getDocument(wikiTitle, false)
        if (result != null) {
          result.score = 1.0
          Some(result)
        } else {
          println("Warn: unable to get oracle: " + query.entityName);
          None
        }
      }
      case None => None
    }
  }


  def getDocumentAsEntity(title: String): Option[WikipediaEntity] = {
    val result = galagoSearcherTacKb.getDocument(title, true)
    if (result == null) {
      None
    } else {
      val galagoEntity = galagoResultToEntity(result, 1.0, galagoSearcherTacKb, "")
      Some(galagoEntity)
    }
  }


  def close() {
    if (writer != None) {
      writer.get.close()
    }

    galagoSearcherTacKb.close();

  }

}

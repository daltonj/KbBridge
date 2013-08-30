package edu.umass.ciir.kbbridge.search

import org.lemurproject.galago.core.retrieval.{ScoredPassage, ScoredDocument}
import edu.umass.ciir.galago._
import org.lemurproject.galago.tupleflow.{FakeParameters, Parameters}
import edu.umass.ciir.kbbridge.search.{EntityRetrievalWeighting, EntityReprRetrieval}
import org.lemurproject.galago.core.parse.{TagTokenizer, Document}
import scala.Some
import edu.umass.ciir.galago.ParametrizedQuery

/**
 * User: dietz
 * Date: 8/17/13
 * Time: 2:54 PM
 */
class SketchRetrieval(val docSearcher: GalagoSearcher, val docSearchParams: Parameters, val kbSearcher: GalagoSearcher,
                      val kbSearchParams: Parameters, val psgSize: Int, val psgShift: Int
                     ) {

  def fixCorpusPassageRetrieval(docs: Seq[ScoredDocument], searchQuery: ParametrizedQuery,
                                killExtentQuery: Boolean = false
                               ): Seq[ScoredPassage] = {
    fixedPassageRetrieval(docs, searchQuery, docSearcher, killExtentQuery = killExtentQuery)
  }

  def fixKbPassageRetrieval(docs: Seq[ScoredDocument], searchQuery: ParametrizedQuery, killExtentQuery: Boolean = false
                           ): Seq[ScoredPassage] = {
    fixedPassageRetrieval(docs, searchQuery, kbSearcher, None, killExtentQuery = killExtentQuery)
    // LD it was a problem with empty text extents
    //    fixedPassageRetrieval(docs, searchQuery, kbSearcher, Some(Freebase2WikipediaMap.wikipediaTitleNormalizationMap))
  }

  def fixedPassageRetrieval(docs: Seq[ScoredDocument], searchQuery: ParametrizedQuery, galagoSearcher: GalagoSearcher,
                            lowercaseNormalizationMap: Option[Map[String, String]] = None, killExtentQuery: Boolean
                           ): Seq[ScoredPassage] = {
    println("Fixing max passage retrieval...")
    val passageQuery = {
      val ParametrizedQuery(str, p) = GalagoQueryBuilder.passageRetrieval(searchQuery, docs.map(_.documentName).toList,
                                                                          passageSize = psgSize,
                                                                          passageShift = psgShift)
      if (killExtentQuery) {
        p.remove("processingModel")
        p.remove("extent")
        p.remove("extentQuery")
      }
      ParametrizedQuery(str, p)
    }

    print("re-running query :" + searchQuery.queryStr + " \n" + searchQuery.parameters.toPrettyString)
    val passagesOpts =
      for (doc <- docs) yield {
        //        println("maxpassage " + doc.documentName)
        passageQuery.parameters.set("working", java.util.Collections.singletonList(doc.documentName))
        val singlePassageResult = galagoSearcher.retrieveScoredPassages(passageQuery.queryStr,
                                                                        Some(passageQuery.parameters), resultCount = 1)
        if (singlePassageResult.headOption.isEmpty) {
          // the wikipediaTitle has wrong capitalization
          // fix and retry
          if (lowercaseNormalizationMap.isDefined) {
            lowercaseNormalizationMap.get.get(doc.documentName.toLowerCase) match {
              case None => {
                println("Could not fix maxpassage document name with lowercaseNormalizationMap " + doc.documentName)
                None
              }
              case Some(correctTitle) => {
                passageQuery.parameters.set("working", java.util.Collections.singletonList(correctTitle))

                val singlePassageResult = galagoSearcher.retrieveScoredPassages(passageQuery.queryStr,
                                                                                Some(passageQuery.parameters),
                                                                                resultCount = 1)
                if (singlePassageResult.headOption.isEmpty) {
                  println("Could not retrieve max passage for document " + doc.documentName + " " + doc.score + " " + doc.rank)
                  None
                } else {
                  println("Fixed! maxpassage " + correctTitle)
                  val psg = singlePassageResult.head
                  Some(psg)
                }
              }
            }
          } else {
            println("Could not retrieve max passage for document " + doc.documentName + " " + doc.score + " " + doc.rank)
            None
          }
        } else {
          val psg = singlePassageResult.head
          Some(psg)
        }
      }

    // passages all have rank 1, and are sorted according to the document ranking
    val sortedPassages = passagesOpts.flatten.sortBy(-_.score)

    for ((sPsg, index) <- sortedPassages.zipWithIndex) {
      val rank = index + 1
      sPsg.rank = rank
    }

    println("... Done fixing max passage retrieval")

    sortedPassages
  }


  def resetKbCache() {
    kbSearcher.resetDocumentCache()
  }

  def getKbDocument(docname: String): Document = {
    kbSearcher.getDocument(docname, kbSearchParams)
  }

  def resetCorpusCache() {
    docSearcher.resetDocumentCache()
  }

  def getCorpusDocument(docname: String): Document = {
    docSearcher.getDocument(docname, docSearchParams)
  }

  def kbFakeTokenize(text: String): Document = {
    val tagTokenizer = new TagTokenizer(new FakeParameters(kbSearchParams))
    tagTokenizer.tokenize(text)
  }

  def corpusFakeTokenize(text: String): Document = {
    val tagTokenizer = new TagTokenizer(new FakeParameters(docSearchParams))
    tagTokenizer.tokenize(text)
  }


}

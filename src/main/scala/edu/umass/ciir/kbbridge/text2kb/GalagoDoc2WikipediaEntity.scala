package edu.umass.ciir.kbbridge.text2kb

import org.lemurproject.galago.core.retrieval.ScoredDocument
import edu.umass.ciir.kbbridge.data.{IdMap, ScoredWikipediaEntity}
import edu.umass.ciir.kbbridge.util.SeqTools
import scala.collection.JavaConversions._
import edu.umass.ciir.kbbridge.search.GalagoRetrieval
import org.lemurproject.galago.core.parse.Document

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 5:20 PM
 */
object GalagoDoc2WikipediaEntity {

  def docToEntity(galagoDoc: Document) : ScoredWikipediaEntity = {
    val dummySd = new ScoredDocument(galagoDoc.name, -1, -1)
    val merged = scoredDocToEntity(dummySd)
    merged
  }

  def idToEntity(id:String) : ScoredWikipediaEntity = {
    val dummySd = new ScoredDocument(id, -1, -1)
  //  val galagoDoc = galago.getDocument(id)
    val merged = scoredDocToEntity(dummySd)
    merged
  }

  def galagoResultToWikipediaEntities(docs: Seq[ScoredDocument]): Seq[ScoredWikipediaEntity] = {
  //  val name2sd = seq.map(elem => (elem.documentName, elem))
   // val name2doc = galago.getDocuments(seq.map(_.documentName))

  //  val merged = SeqTools.alignMaps(name2sd, name2doc)
    for (d <- docs) yield {
      scoredDocToEntity(d)
    }

  }
  def scoredDocToEntity(sd: ScoredDocument): ScoredWikipediaEntity = {
    val wikititle = sd.documentName
    val score = sd.score
    val rank = sd.rank
    val wikiId = IdMap.wikiTitle2WikiIdMap(wikititle)

    //    val tacIdListOpt = IdMap.wikiTitle2TacIdMap.get(wikititle)

    new ScoredWikipediaEntity(
      wikititle
      , wikipediaId = wikiId
      , score = score
      , rank = rank
    )

  }
}

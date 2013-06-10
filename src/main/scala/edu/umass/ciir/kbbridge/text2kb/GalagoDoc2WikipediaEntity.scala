package edu.umass.ciir.kbbridge.text2kb

import org.lemurproject.galago.core.retrieval.ScoredDocument
import edu.umass.ciir.kbbridge.data.{IdMap, ScoredWikipediaEntity}
import edu.umass.ciir.kbbridge.util.SeqTools
import scala.collection.JavaConversions._
import edu.umass.ciir.kbbridge.search.GalagoRetrieval

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 5:20 PM
 */
class GalagoDoc2WikipediaEntity(galago:GalagoRetrieval) {


  def galagoResultToWikipediaEntities(seq: Seq[ScoredDocument]): Seq[ScoredWikipediaEntity] = {
    val name2sd = seq.map(elem => (elem.documentName, elem))
    val name2doc = galago.getDocuments(seq.map(_.documentName))

    val merged = SeqTools.alignMaps(name2sd, name2doc)
    for ((key, (sd, galagoDoc))<- merged) yield {
      galagoMergedDocToWikipediaEntity(sd, galagoDoc)
    }

  }
  def galagoMergedDocToWikipediaEntity(sd: ScoredDocument, galagoDoc:org.lemurproject.galago.core.parse.Document): ScoredWikipediaEntity = {
    val wikititle = sd.documentName
    val score = sd.score
    val rank = sd.rank

    val metadata:Map[String,String] = {
      val m = galagoDoc.metadata
      if (m == null) Map.empty
      else m.toMap
    }
//    val text = {
//      val t = galagoDoc.text
//      if (t == null) ""
//      else t
//    }


    val wikiId = {
      if (metadata.size > 0) {
        metadata.getOrElse("wikiId","0").toInt
      } else {
        IdMap.wikiTitle2WikiIdMap(wikititle)
      }
    }

    //    val tacIdListOpt = IdMap.wikiTitle2TacIdMap.get(wikititle)

    new ScoredWikipediaEntity(
      wikititle
      , wikipediaId = wikiId
      , metadata = metadata
      , score = score
      , rank = rank
    )

  }
}

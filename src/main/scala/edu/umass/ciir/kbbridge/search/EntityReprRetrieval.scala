package edu.umass.ciir.kbbridge.search

import scala.Predef._
import scala.Double
import edu.umass.ciir.models.StopWordList
import scala.collection.JavaConversions._
import edu.umass.ciir.memindex.Query
import org.lemurproject.galago.core.tools.Search.SearchResultItem
import org.lemurproject.galago.core.tools.Search
import edu.umass.ciir.kbbridge.util.{SeqTools, ConfInfo}
import edu.umass.ciir.kbbridge.data.repr.EntityRepr
import edu.umass.ciir.galago.GalagoQueryLib
import edu.umass.ciir.kbbridge.data.repr.EntityRepr
import org.lemurproject.galago.core.retrieval.ScoredDocument

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 2:52 PM
 */

case class EntityRetrievalWeighting(lambdaQ:Double=1.0, lambdaV:Double=1.0, lambdaS:Double=0.0, lambdaM:Double=0.0)

object Dmp{
def dmp(args:String) {
  println(args)
}}

class EntityReprRetrieval(val galago:GalagoRetrieval, val entityRetrievalWeighting:EntityRetrievalWeighting, val x :String = "bla", val queryDumper:(String=> Unit) = Dmp.dmp) {


  def search(entity:EntityRepr, numResults:Int): Seq[ScoredDocument] = {
//    p.set("odw", 0.21D)
//    p.set("uniw", 0.29D)
//    p.set("uww", 0.50D)

    val fullQuery: String = buildRawQuery(entity)

    println(fullQuery)

    galago.retrieveScoredDocuments(fullQuery, numResults)
  }


  def searchEntitySetWithQuery(queryStr:String, weightedEntities:Seq[(EntityRepr, Double)], origWeight:Double, numResults:Int): Seq[ScoredDocument] = {
    //    p.set("odw", 0.21D)
    //    p.set("uniw", 0.29D)
    //    p.set("uww", 0.50D)


    val fullQuery: String = buildRawQueryFromEntitySetWithQuery(weightedEntities, queryStr, origWeight)
    //println(fullQuery)
    queryDumper(fullQuery)

    galago.retrieveScoredDocuments(fullQuery, numResults)
  }


  def buildRawQueryFromEntitySetWithQuery(weightedEntities: Seq[(EntityRepr, Double)], queryStr: String, origWeight: Double): String = {
    val weightedEntityQueries =
      for ((entityRepr, weight) <- weightedEntities) yield {
//        println("\t"+weight+"\t" +buildRawQuery(entityRepr) )
        buildRawQuery(entityRepr) -> weight
      }
    val entityQuery: String =
      GalagoQueryLib.buildWeightedCombine(weightedEntityQueries)

    val fullQuery = GalagoQueryLib.buildWeightedCombine(Seq(queryStr -> origWeight, entityQuery -> (1.0 - origWeight)))
    fullQuery
  }

  def buildRawQuery(entity: EntityRepr): String = {
    val queryQ = GalagoQueryLib.buildSeqDepForString(entity.entityName)
    val queryNV = {
      val innerQueries =
        for ((nv, weight) <- entity.nameVariants.toSeq) yield {
          GalagoQueryLib.buildSeqDepForString(nv) -> weight
        }
      GalagoQueryLib.buildWeightedCombine(innerQueries)
    }

    val queryM = {
      val innerQueries =
        for ((neighbor, weight) <- entity.neighbors.toSeq) yield {
          GalagoQueryLib.buildSeqDepForString(neighbor.entityName) -> weight
        }
      GalagoQueryLib.buildWeightedCombine(innerQueries)
    }

    val queryS = {
      GalagoQueryLib.buildWeightedCombine(entity.words.toSeq)
    }

//    println ("queryQ "+queryQ)
//    println ("queryNV "+queryNV)
//    println ("queryM "+queryM)
//    println ("queryS "+queryS)

    val fullQuery =
      GalagoQueryLib.buildWeightedCombine(Seq(
        queryQ -> entityRetrievalWeighting.lambdaQ,
        queryNV -> entityRetrievalWeighting.lambdaV,
        queryS -> entityRetrievalWeighting.lambdaS,
        queryM -> entityRetrievalWeighting.lambdaM
      ))
    fullQuery
  }
}

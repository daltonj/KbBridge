package edu.umass.ciir.kbbridge

import data.repr.EntityRepr
import data.{ScoredWikipediaEntity, WikipediaEntity}
import search.{RetrievalMap, EntityRetrievalWeighting, GalagoRetrieval, EntityReprRetrieval}
import util.{ConfInfo, KbBridgeProperties}
import text2kb.{GalagoDoc2WikipediaEntity, TextEntityReprGenerator}

object EntityLinkerMain {

  val nilThreshold = -10

  val reprGenerator = new TextEntityReprGenerator()
  val galago = RetrievalMap.getSearcher
  val candidateGenerator = new EntityReprRetrieval(galago, EntityRetrievalWeighting(0.5, 0.25, 0.05, 0.2))

  val reranker = new RankLibReranker(KbBridgeProperties.rankerModelFile)

  def link(entityRepr: EntityRepr): Option[WikipediaEntity] = {

    val searchResult = candidateGenerator.search(entityRepr, 10)
    val cands =GalagoDoc2WikipediaEntity.galagoResultToWikipediaEntities(searchResult)

    for (cand <- cands){
      println(cand.wikipediaTitle+"  "+cand.score+" "+cand.rank+"\n")
//      printCandidateInfo(cand)
    }


    if (cands.size > 0) {
      println("Linking result:\tquery: " + entityRepr.entityName + " " + "\ttop cand: " + cands.head.wikipediaTitle + "\treranked: "
        + cands.head.wikipediaTitle + "\tscore: " + cands.head.score + "\tNIL?: " + (if (cands.head.score > nilThreshold) false else true))

      println("Features:\n"+cands.head.featureMap.map(_.toSeq.map(_.toString()).mkString("\n")))

      if (cands.head.score > nilThreshold) Some(cands.head) else None
    } else {
      println("Linking result: query: " + entityRepr.entityName + " " + "top cand: NIL reranked: NIL")
      None
    }
  }

//  def printCandidateInfo(cand: ScoredWikipediaEntity) {
//    for ((key, v) <- cand.metadata; if key != "xml") {
//      if (key == "contextLinks") {
//        println(key + "\t\t" + (v.split("\n").take(10).mkString("", "\n", "...\n")))
//      } else
//        println(key + "\t\t" + v)
//    }
//  }


  def main(args: Array[String]) {

    KbBridgeProperties.loadProperties("./config/kbbridge.properties")
    println("Ranker model: " + KbBridgeProperties.rankerModelFile)

    val testEntity = EntityRepr(
      "Andre Agassi"
      , Some("test")
      , Seq("Agassi" -> 1.0)
      , neighbors =Seq(EntityRepr("Wimbledon") -> 0.5, EntityRepr("Steffi Graf") -> 0.5)
      , words =Seq("tennis" -> 1.0)
    )
    link(testEntity)

  }
}
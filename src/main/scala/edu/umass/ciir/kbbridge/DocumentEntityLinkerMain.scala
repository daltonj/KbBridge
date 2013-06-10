package edu.umass.ciir.kbbridge

import edu.umass.ciir.kbbridge.data.{SimpleEntityMention, EntityMention, WikipediaEntity}
import search.{RetrievalMap, EntityRetrievalWeighting, GalagoRetrieval, EntityReprRetrieval}
import util.{ConfInfo, KbBridgeProperties}
import text2kb.{KnowledgeBaseCandidateGenerator, GalagoDoc2WikipediaEntity, TextEntityReprGenerator}

object DocumentEntityLinkerMain {

  val nilThreshold = -10


  val reranker = new RankLibReranker(KbBridgeProperties.rankerModelFile)

  def link(query: EntityMention): Option[WikipediaEntity] = {

    val cands = KnowledgeBaseCandidateGenerator.apply().retrieveCandidates(query, 10)

    for (cand <- cands){
      println(cand.wikipediaTitle+"  "+cand.score+" "+cand.rank+"\n")
//      for((key,v) <- cand; if key != "xml") {
//        if (key == "contextLinks"){
//          println(key + "\t\t"+ (v.split("\n").take(10).mkString("","\n","...\n")))
//        } else
//          println(key +"\t\t"+v)
//      }
////        +cand.metadata)
    }


    val reranked = reranker.rerankCandidatesGenerateFeatures(query, cands)

    if (reranked.size > 0) {
      println("Linking result:\tquery: " + query.entityName + " " + "\ttop cand: " + cands.head.wikipediaTitle + "\treranked: "
        + reranked.head.wikipediaTitle + "\tscore: " + reranked.head.score + "\tNIL?: " + (if (reranked.head.score > nilThreshold) false else true))

      println("Features:\n"+reranked.head.featureMap.map(_.toSeq.map(_.toString()).mkString("\n")))

      if (cands.head.score > nilThreshold) Some(reranked.head) else None
    } else {
      println("Linking result: query: " + query.entityName + " " + "top cand: NIL reranked: NIL")
      None
    }
  }


  def main(args: Array[String]) {

    KbBridgeProperties.loadProperties("./config/kbbridge.properties")
    println("Ranker model: " + KbBridgeProperties.rankerModelFile)

    val testEntity = new SimpleEntityMention("test", entityType = "PERSON", mentionId = "test01", entityName = "Bill Clinton", fullText = "William Jefferson \"Bill\" Clinton is an American politician who served as the 42nd President of the United States from 1993 to 2001. Inaugurated at age 46, he was the third-youngest president.")
    link(testEntity)

  }
}
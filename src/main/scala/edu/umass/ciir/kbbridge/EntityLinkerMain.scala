import edu.umass.ciir.kbbridge.data.{SimpleEntityMention, EntityMention, WikipediaEntity}
import edu.umass.ciir.kbbridge.RankLibReranker
import edu.umass.ciir.kbbridge.search.GalagoCandidateGenerator
import edu.umass.ciir.kbbridge.util.KbBridgeProperties

object EntityLinkerMain {

  val nilThreshold = -10

  val candidateGenerator = new GalagoCandidateGenerator()

  val reranker = new RankLibReranker(KbBridgeProperties.rankerModelFile)

  def link(query: EntityMention): Option[WikipediaEntity] = {
    val cands = candidateGenerator.findCandidateEntities(query, 50)
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

    val testEntity = new SimpleEntityMention("test", "PERSON", "test01", "Bill Clinton", "")
    link(testEntity)

  }
}
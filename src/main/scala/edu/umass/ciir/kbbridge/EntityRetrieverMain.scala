package edu.umass.ciir.kbbridge

import data.repr.EntityRepr
import data.{GalagoBridgeDocumentWrapper, GalagoBridgeDocument, WikipediaEntity}
import kb2text.{BridgeWikiEntityRepr, GalagoDoc2BridgeDoc}
import search.{GalagoDoc2RetrievedText, GalagoRetrieval, EntityRetrievalWeighting, EntityReprRetrieval}
import util.{ConfInfo, KbBridgeProperties}
import text2kb.{QVSMLocalTextEntityRepr, GalagoDoc2WikipediaEntity}

object EntityRetrieverMain {

  val nilThreshold = -10

  val galago = new GalagoRetrieval(
    jsonConfigFile= ConfInfo.galagoDefaultJsonParameterFile,
    galagoUseLocalIndex = true
  )
  val galagoToBridgeDoc: GalagoDoc2RetrievedText = GalagoDoc2BridgeDoc.createDocConverter(galago)
  val candidateGenerator = new EntityReprRetrieval(galago, EntityRetrievalWeighting(0.5, 0.25, 0.05, 0.2))

//  val reranker = new RankLibReranker(KbBridgeProperties.rankerModelFile)


  def buildEntityRepr(wikipediaTitle:String):EntityRepr = {
    val bridgeDocForEntity = new GalagoBridgeDocumentWrapper(documentname = wikipediaTitle, galagoDocument = None)

    BridgeWikiEntityRepr.buildEntityRepr(wikipediaTitle, bridgeDocForEntity)
  }

  def retrieve(entityRepr: EntityRepr) {
    val searchResult = candidateGenerator.search(entityRepr, 10)
    val cands =galagoToBridgeDoc.galagoResultToRetrievedText(searchResult)

    for (cand <- cands){
      println(cand.documentname+"  "+cand.rawScore+" "+cand.rank+"\n")
      println(cand.terms.take(250).mkString(" "))
    }

  }


  def main(args: Array[String]) {

    KbBridgeProperties.loadProperties("./config/kbbridge.properties")

//    val testEntity = EntityRepr(
//      "Andre Agassi"
//      , Some("test")
//      , Seq("Agassi" -> 1.0)
//      , neighbors =Seq(EntityRepr("Wimbledon") -> 0.5, EntityRepr("Steffi Graf") -> 0.5)
//      , words =Seq("tennis" -> 1.0)
//    )

//    val testEntity = buildEntityRepr("Andre_Agassi")
    val testEntity = buildEntityRepr("Nina_Burleigh")
    retrieve(testEntity)

  }
}
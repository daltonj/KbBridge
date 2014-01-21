package edu.umass.ciir.kbbridge.data

import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention
import edu.umass.ciir.kbbridge.tac.{TacFileMap, TacDocumentXmlLoader}
import edu.umass.ciir.kbbridge.util.ConfInfo
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention

/**
 * User: jdalton
 * Date: 3/29/13
 */
case class TacEntityMention(override val docId: String,
                            override val entityType: String,
                            override val mentionId: String,
                            override val entityName: String,
                            override val corefChain: Seq[NlpXmlNerMention] = Seq(),
                            nodeId: String,
                            override val groundTruth:String,
                            override val nerNeighbors: Seq[NlpXmlNerMention],
                            var text: Option[String] = None,
                            override val linkedMentions : Seq[LinkedMention] = Seq())

  extends EntityMention(docId, entityType, mentionId, entityName, corefChain, groundTruth, nerNeighbors) {

  def fullText: String = {
   val docText = text match {
      case Some(docText) => docText
      case None =>   {
        val fileName = ConfInfo.sourceDir + TacFileMap.docIdToFilenameMap(docId)
        TacDocumentXmlLoader.fullTextViaTacSource(fileName)
      }
    }
    docText
  }

  def groundTruthWikiTitle: Option[String] = {
    if (nodeId.toLowerCase startsWith "nil") {
      None
    } else {
      IdMap.tacId2WikiTitleMap.get(nodeId)
    }
  }

  def isNilQuery = {
    if (nodeId.toLowerCase startsWith "nil") {
      true
    } else {
      // the wikipedia page was deleted!
      groundTruthWikiTitle match {
        case Some(title) => false
        case None => true
      }
    }
  }

  def isTACNilQuery = {
    if (nodeId.toLowerCase startsWith "nil") {
      true
    } else {
      false
    }
  }
}

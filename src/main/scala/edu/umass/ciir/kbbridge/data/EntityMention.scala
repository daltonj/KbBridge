package edu.umass.ciir.kbbridge.data

import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention

/**
 * User: jdalton
 * Date: 3/29/13
 */
abstract class EntityMention(val docId:String, val entityType:String, val mentionId:String, val entityName:String, val corefChain:Seq[NlpXmlNerMention]=Seq(), val groundTruth:String, val nerNeighbors: Seq[NlpXmlNerMention]=Seq(), val linkedMentions : Seq[LinkedMention] = Seq()) {

def fullText:String
}

case class SimpleEntityMention(override val docId: String, override val entityType: String, override val mentionId: String,
override val entityName: String,  override val fullText:String, override val corefChain: Seq[NlpXmlNerMention] = Seq(), override val nerNeighbors :Seq[NlpXmlNerMention]=Seq(), override val groundTruth:String="", override val linkedMentions : Seq[LinkedMention] = Seq())
extends EntityMention(docId, entityType, mentionId, entityName, corefChain, groundTruth, nerNeighbors)
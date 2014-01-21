package edu.umass.ciir.kbbridge.nil

import scala.collection.mutable.ListBuffer
import collection.mutable.HashMap
import edu.umass.ciir.kbbridge.eval.TacMetricsCalculator.LinkerQueryPrediction
import edu.umass.ciir.kbbridge.text2kb.{QVTextEntityRepr, QVMLocalTextEntityRepr}
import edu.umass.ciir.kbbridge.data._
import java.io.File
import scala.xml.XML
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention
import scala.Some
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention
import edu.umass.ciir.kbbridge.eval.TacMetricsCalculator.LinkerQueryPrediction
import edu.umass.ciir.kbbridge.data.SimpleEntityMention
import scala.Some
import edu.umass.ciir.kbbridge.data.TacEntityMention
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention
import edu.umass.ciir.kbbridge.eval.TacMetricsCalculator.LinkerQueryPrediction

object NilClusterer {

  var lastNil = 0
  var queryNameToNilNumber = new HashMap[String, Int]()

  def clusterNils(predictions: Seq[LinkerQueryPrediction]): Seq[LinkerQueryPrediction] = {

    val finalResults = new ListBuffer[LinkerQueryPrediction]

    for (p <- predictions) {
      val aid = p.tacId match {
        case None => {
          if (p.wikiTitle.isDefined && p.wikiTitle.get.length() > 0) {
            // first use nilcluster
            List("NIL" + nextNilNumber("##" + p.wikiTitle.get))
          } else {
            // if no nilcluster given, use the query name

            val file = new File("/usr/aubury/scratch1/jdalton/data/tac-nlp-annotations-2013-samfactoriener3/" + p.query.docId +".xml")
            val prediction = if (file.exists()) {
              val annotatedMention = annotationsFromFile(p.query, file)
              val repr = QVTextEntityRepr.createEntityRepr(annotatedMention)
             // println("NIL doc: " + p.query + " name variants:" + repr.nameVariants)
              if (repr.nameVariants.size == 1) {
                List("NIL" + nextNilNumber("--" + repr.nameVariants.head._1))
              } else {
                List("NIL" + nextNilNumber("--" + p.query.mentionId))
              }
            } else {
              List("NIL" + nextNilNumber("--" + p.query.mentionId))
            }

            prediction
            //"NIL"+nextNilNumber("--"+p.queryName)
          }
        }
        case Some(an) => an
      }
      finalResults += new LinkerQueryPrediction(p.query, Some(aid), p.wikiTitle, p.score)
    }
    finalResults
  }

  def nextNilNumber(qname: String): String = {
    val oldNil = queryNameToNilNumber.get(qname)
    oldNil match {
      case None => {
        lastNil += 1

        queryNameToNilNumber += (qname -> lastNil)
        "%04d".format(lastNil)
      }
      case Some(recycledNil) => {
        "%04d".format(recycledNil)
      }
    }
  }


  def annotationsFromFile(m: EntityMention, file: File) = {

    val xmlDoc = XML.loadFile(file)

    val entityLinks = xmlDoc \\ "entitylink"
    val linkedMentions = entityLinks.map(e => {
      //println((e \ "name").text)
      val mention = new SimpleEntityMention(m.docId, "", (e \ "mentionId").text, (e \ "name").text, "")
      val candidates = e \\ "candidate" take 10
      val candidateEntities = for (c <- candidates) yield {
        // println(c)
        val id = (c \ "id").text.trim
        val score = (c \ "score").text.trim.toDouble
        val rank = (c \ "rank").text.trim.toInt
        new ScoredWikipediaEntity(id, -1, score, rank)
      }
      val linkedMention = LinkedMention(mention, candidateEntities)
      // println(linkedMention)
      linkedMention
    })


    val tokens = xmlDoc \\ "token"
    val text = tokens.map(t => (t \ "word").text.trim()).mkString(" ")

    val mentions = xmlDoc \\ "mention"

    val neighbors = mentions.map(mention => {
      val span = (mention \ "string").text.trim()
      val tokenStart = (mention \ "TokenBegin").text.trim().toInt
      val tokenEnd = (mention \ "TokenEnd").text.trim().toInt
      val charStart = (mention \ "CharacterOffsetBegin").text.trim().toInt
      val charEnd = (mention \ "CharacterOffsetBegin").text.trim().toInt
      val eType = (mention \ "type").text.trim()
      new NlpXmlNerMention(span, Seq(), -1, false, tokenStart, tokenEnd, charStart, charEnd, eType)

    })

    val titleOption = IdMap.tacId2WikiTitleMap.get("blah")
    val title = titleOption match {
      case Some(titleOption) => titleOption
      case None => ""
    }
   // println("Num neighbors: " + neighbors.size + " num links: " + linkedMentions.size)
    val newMention = TacEntityMention(docId = m.docId
      , entityType = m.entityType
      , mentionId = m.mentionId
      , entityName = m.entityName
      , nodeId = "NIL"
      , groundTruth = title,
      nerNeighbors = neighbors,
      text = Some(text),
      linkedMentions = linkedMentions
    )

    newMention

  }

}
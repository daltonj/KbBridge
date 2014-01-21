package edu.umass.ciir.kbbridge

import edu.umass.ciir.kbbridge.search.{EntityRetrievalWeighting, EntityReprRetrieval, GalagoRetrieval}
import org.lemurproject.galago.tupleflow.Parameters
import edu.umass.ciir.kbbridge.util.ConfInfo
import edu.umass.ciir.kbbridge.tac.TacQueryUtil
import edu.umass.ciir.kbbridge.data.{IdMap, ScoredWikipediaEntity, SimpleEntityMention, TacEntityMention}
import java.io.{PrintWriter, File}
import edu.umass.ciir.galago.{GalagoSearcher, GalagoQueryLib}
import org.lemurproject.galago.core.retrieval.ScoredDocument
import edu.umass.ciir.kbbridge.trec.TrecRunWriter
import scala.xml.XML
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention
import edu.umass.ciir.kbbridge.text2kb.{QVMLocalTextEntityRepr, TextEntityReprGenerator}

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 9/10/13
 * Time: 3:37 PM
 */
object CorpusExpander extends App {

  val indexPath = args(0)
  val params = new Parameters()
  val retrieval : GalagoSearcher = GalagoSearcher(indexPath)
  val querySet = TacQueryUtil.non2009Queries

  val inputNlpDir = args(1)
  val outputDir = args(2)
  val runId = args(3)

  val numResults = 100

  val results = runQueries(querySet)

  val uniqDocs = results.values.flatten.map(_.documentName).toSet

  writeAnnotationScript(uniqDocs)

  TrecRunWriter.writeRunFile(new File(outputDir + "/" + runId + ".run"), results)

  def runQueries(querySet : Seq[TacEntityMention])  : Map[String, Seq[ScoredDocument]] = {

    val allResults = for ((m,idx) <- querySet.zipWithIndex) yield {

      val file = new File(inputNlpDir + File.separatorChar + m.docId + ".xml")
      val query = if (file.exists()) {
        val annotatedMention = annotationsFromFile(m, file)
        val repr = QVMLocalTextEntityRepr.createEntityRepr(annotatedMention)


        val query = if (repr.nameVariants.size > 0) {
          val queryString = EntityReprRetrieval.buildRawQuery(repr,EntityRetrievalWeighting(0.31, 0.38, 0.0, 0.31), false, 0)
          queryString
        } else {
          val queryString = EntityReprRetrieval.buildRawQuery(repr,EntityRetrievalWeighting(0.7, 0.0, 0.0, 0.3), false, 0)
          queryString
        }

        query
      } else {
        println("annotation file does not exist, using name query")
        val galagoQuery = GalagoQueryLib.buildSeqDepForString(m.entityName, Seq())
        galagoQuery
      }


     // val galagoQuery = GalagoQueryLib.buildSeqDepForString(m.entityName, Seq(), false, 0)
      println("Running query:"+ idx + " of "  + querySet.size + " " + m.mentionId + " " + m.docId + " " + query)
      val results = try {
        val results = retrieval.retrieveScoredDocuments(query, None, numResults)
        results
      } catch {
        case e =>  retrieval.retrieveScoredDocuments(GalagoQueryLib.buildSeqDepForString(m.entityName, Seq()), None, numResults)
      }

      m.mentionId -> results
    }
    allResults.toMap
  }




  def annotationsFromFile(m: TacEntityMention, file: File) = {

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

    val titleOption = IdMap.tacId2WikiTitleMap.get(m.nodeId)
    val title = titleOption match {
      case Some(titleOption) => titleOption
      case None => ""
    }
    println("Num neighbors: " + neighbors.size + " num links: " + linkedMentions.size)
    val newMention = TacEntityMention(docId = m.docId
      , entityType = m.entityType
      , mentionId = m.mentionId
      , entityName = m.entityName
      , nodeId = m.nodeId
      , groundTruth = title,
      nerNeighbors = neighbors,
      text = Some(text),
      linkedMentions = linkedMentions
    )

    newMention

  }

  def writeAnnotationScript(docsRequiringAnnotation: Iterable[String]) = {
    val outputFile = new File("./scripts/annotate-fully-tacexpansion-qvm-swarm-factkb1")
    val n = 100000
    var curBatch = 0
    var p = new PrintWriter(outputFile.getAbsolutePath() + curBatch.toString + ".sh", "UTF-8")
    for ((docSet, idx) <- (docsRequiringAnnotation grouped 1).zipWithIndex) {
      val sb = new StringBuilder
      if (idx % n == 0 && idx > 0) {
        p.close
        curBatch += 1
        p = new PrintWriter(outputFile.getAbsolutePath() + curBatch.toString + ".sh", "UTF-8")
      }

      sb append "qsub -b y " + "-l mem_free=6G -l mem_token=6G" + " -cwd -o ./out/"
      sb append docSet.head
      sb append " -e ./err/"
      sb append docSet.head

      sb append " java -server -Xmx6G -Dfile.encoding=utf-8 -cp /work1/allan/jdalton/factorie-kbbridge-plugin/target/factorie-kbbridge-1.0-SNAPSHOT-jar-with-dependencies.jar cc.factorie.app.nlp.el.TacLinkingMain "
      //  sb append " /work1/allan/jdalton/tacco/scripts/runEntityLinker.sh "

      // input query
      sb append docSet.mkString(",")
      sb append " /work1/allan/jdalton/entity-linking/tac-source2013-g34"
      sb append " ./tac-10expansiondocs-annotations-allyears-factkb1"

      // println(sb.toString)
      p.println(sb.toString)
    }
    p.close()
  }

}

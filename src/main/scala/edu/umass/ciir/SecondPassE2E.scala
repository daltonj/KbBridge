package edu.umass.ciir.kbbridge

import edu.umass.ciir.kbbridge.tac.TacQueryUtil
import java.io.{File}
import edu.umass.ciir.kbbridge.data.{IdMap, ScoredWikipediaEntity, SimpleEntityMention, TacEntityMention}
import scala.xml.XML
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 9/15/13
 * Time: 3:33 PM
 */
object SecondPassE2E extends App {

  var tacQueryIds: Set[String] = args(0).split(",").toSet

  val outputDir: File = new File(args(4))
  val e2eModelFile = args(3)

  val reranker = new RankLibReranker(e2eModelFile, "queryonly,namevar,localcontext,galago,e2e".split(","), "./data/ltr/domainMap_e2e")

  if (!outputDir.exists()) {
    outputDir.mkdirs()
  }
  println("Starting to load query files.")

  val querySet = tacQueryIds

  val tacQueries = TacQueryUtil.allQueries()

  val testQueries = if (tacQueryIds.size > 0) {
    tacQueries.filter(q => tacQueryIds contains q.mentionId)
  } else {
    tacQueries
  }

  for (q <- testQueries) yield {

    // firstPass directory
    val firstPassFile = new File(args(1) + File.separatorChar + q.docId + ".xml")

    // first pass - only mention results
    val mentionOnlyFile = new File(args(2) + File.separatorChar + q.mentionId + ".xml")

    if (firstPassFile.exists() && mentionOnlyFile.exists()) {
      val annotatedMention = annotationsFromFile(q, firstPassFile, 20)
      val candidates = entityLinks(q.docId, mentionOnlyFile, 20).head.entityLinks
      val rerankede2e = reranker.rerankCandidatesGenerateFeatures(annotatedMention, candidates)

      val xml = LinkedMention2XmlRenderer.xml(q, rerankede2e, candidates)

      val outputFile = new File(outputDir.getAbsolutePath + File.separatorChar + q.mentionId + ".xml")
      XML.save(outputFile.getAbsolutePath, xml, "UTF-8")
    } else {
      println("Unable to load mention files for query: " + q)

    }
  }


  def entityLinks(docId: String, file: File, candidateLimit: Int = 50) = {
    val entityLinks = XML.loadFile(file) \\ "entitylink"
    for (e <- entityLinks) yield {
      //println((e \ "name").text)
      val mention = new SimpleEntityMention(docId, "", (e \ "mentionId").text, (e \ "name").text, "")
      val candidates = e \\ "candidate" take candidateLimit
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
    }
  }


  object LinkedMention2XmlRenderer {

    def xml(m: TacEntityMention, links: Seq[ScoredWikipediaEntity], rawRank: Seq[ScoredWikipediaEntity]) = {
      <root>
        <document>
          <name>
            {m.docId}
          </name>
          <mention>
            <id>
              {m.mentionId}
            </id>
            <string>
              {m.entityName}
            </string>
            <type>
              {m.entityType}
            </type>
            <CharacterOffsetBegin>
              {-1}
            </CharacterOffsetBegin>
            <CharacterOffsetEnd>
              {-1}
            </CharacterOffsetEnd>
            <TokenBegin>
              {-1}
            </TokenBegin>
            <TokenEnd>
              {-1}
            </TokenEnd>
          </mention>
          <kblinks>
            <entitylink>
              <name>
                {m.entityName}
              </name>
              <CharacterOffsetBegin>
                {-1}
              </CharacterOffsetBegin>
              <CharacterOffsetEnd>
                {-1}
              </CharacterOffsetEnd>
              <TokenBegin>
                {-1}
              </TokenBegin>
              <TokenEnd>
                {-1}
              </TokenEnd>{for (c <- links) yield
              <candidate>
                <id>
                  {c.wikipediaTitle}
                </id>
                <rank>
                  {c.rank}
                </rank>
                <score>
                  {c.score}
                </score>
              </candidate>}
            </entitylink>{for (c <- rawRank) yield
            <rawcandidate>
              <id>
                {c.wikipediaTitle}
              </id>
              <rank>
                {c.rank}
              </rank>
              <score>
                {c.score}
              </score>
            </rawcandidate>}
          </kblinks>
        </document>
      </root>
    }
  }


  def annotationsFromFile(m: TacEntityMention, file: File, numCandidates: Int = 50) = {

    val xmlDoc = XML.loadFile(file)

    val entityLinks = xmlDoc \\ "entitylink"
    val linkedMentions = entityLinks.map(e => {
      //println((e \ "name").text)
      val mention = new SimpleEntityMention(m.docId, "", (e \ "mentionId").text, (e \ "name").text, "")
      val candidates = e \\ "candidate" take numCandidates
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

}

package edu.umass.ciir.kbbridge

import java.io.{PrintWriter, File, FileOutputStream}
import edu.umass.ciir.kbbridge.serial.EntityMentionProtos.{LinkerFeature, ScoredWikipediaEntityFeatures, EntityMentionLinkerFeatures, TacEntityMentionLinkerFeatures}
import edu.umass.ciir.kbbridge.features.{EntityFeaturesToSvmConverter, Mention2EntityFeatureHasher}
import edu.umass.ciir.kbbridge.util.{KbBridgeProperties, WikiLinkExtractor, ConfInfo}
import com.google.protobuf.TextFormat
import edu.umass.ciir.kbbridge.data.{IdMap, ScoredWikipediaEntity, SimpleEntityMention, TacEntityMention}
import edu.umass.ciir.kbbridge.tac.TacQueryUtil
import edu.umass.ciir.kbbridge.text2kb.KnowledgeBaseCandidateGenerator
import scala.xml.XML
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention
import edu.umass.ciir.kbbridge.search.DocumentBridgeMap
import scala.collection.JavaConversions._

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 9/11/13
 * Time: 6:13 PM
 */
object ContextFeatureExtractor extends App {


  var tacQueryIds: Set[String] = args(0).split(",").toSet
  val outputDir: File = new File(args(2))

  if (!outputDir.exists()) {
    outputDir.mkdirs()
  }
  println("Starting to load query file.")
  val tacQueries = TacQueryUtil.allQueries()

  //  val p = new Parameters()
  //  p.set("index", args(1))
  //  val retrieval = RetrievalFactory.instance(p)

  val testQueries = if (tacQueryIds.size > 0) {
    tacQueries.filter(q => tacQueryIds contains q.mentionId)
  } else {
    tacQueries
  }
  println("queries: " + testQueries.size)

  extractFeatures(testQueries)

  def extractFeatures(mentions: Seq[TacEntityMention]) {

    val candidateGenerator = KnowledgeBaseCandidateGenerator()
    val reranker = new RankLibReranker(KbBridgeProperties.rankerModelFile, "queryonly,namevar,localcontext,galago".split(","))

    for (m <- mentions) {
      println("Fetching candidates for mention: " + m.mentionId + " d:" + m.docId + " name:" + m.entityName)
      println(m + " " + m.groundTruthWikiTitle)
      //annotateDocument(m, retrieval)

      val file = new File(args(1) + File.separatorChar + m.docId + ".xml")
      if (file.exists()) {
        val annotatedMention = annotationsFromFile(m, file)
        val candidates = candidateGenerator.retrieveCandidates(annotatedMention, 250)
        val rerankedResults = reranker.rerankCandidatesGenerateFeatures(annotatedMention, candidates).toSeq take 20
        println("reranked candidates: " + rerankedResults.size)

        featuresToProto(annotatedMention, rerankedResults)
      } else {
        println("Annotation file does not exist for query: " + m.mentionId)
      }

    }
  }


  //  def annotateDocument(m : TacEntityMention, retrieval:Retrieval) = {
  //    val nlpSteps = Seq(
  //
  //      // Truecasing??
  //      POS1,
  //      // LemmaAnnotator,
  //      NER1,
  //      //FactorieNERComponent,
  //      //DepParser1,
  //      NerAndPronounMentionFinder
  //    )
  //
  //    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
  //    for (annotator <- nlpSteps) map += annotator
  //    val pipeline = DocumentAnnotatorPipeline(map = map.toMap, prereqs = Nil, nlpSteps.flatMap(_.postAttrs))
  //
  //    var gDoc = retrieval.getDocument(m.docId, p)
  //
  //    if (gDoc == null) {
  //      gDoc = retrieval.getDocument(m.docId.toLowerCase(), p)
  //    }
  //
  //    if (gDoc != null) {
  //      println("Annotating document: " + m.mentionId + " doc:" + m.docId + " name:" + m.entityName)
  //
  //
  //      //val docXml = XML.loadString(gDoc.text)
  //      // val newsDoc = Text2FactorieDoc.newswire(b)
  //      val doc = if (!(m.docId startsWith "bolt")) {
  //        val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
  //        val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
  //        val xmlDoc = adapter.loadXML(new InputSource(new StringReader(gDoc.text)), parser)
  //        // println(xmlDoc.toString())
  //        val text = xmlDoc \\ "TEXT"
  //        // println(text.text)
  //
  //
  //        val headline = xmlDoc \\ "HEADLINE"
  //        val doc = Text2FactorieDoc.news(headline, text)
  //        doc
  //      } else {
  //        Bolt2FactorieDoc.text2FactorieDoc(gDoc.text)
  //      }
  //      // val doc = new Document(textDoc)
  //
  //
  //      doc.setName(gDoc.name)
  //      pipeline.process(doc)
  //
  //      val text = doc.tokens.map(t => t.string).mkString(" ")
  //
  //      val newMention = TacEntityMention(docId = m.docId
  //        , entityType = m.entityType
  //        , mentionId = m.mentionId
  //        , entityName = m.entityName
  //        , nodeId = m.nodeId
  //        , groundTruth = "",
  //        nerNeighbors = extractNerNeighborhood(doc),
  //        text = Some(text) )
  //      newMention
  //    }  else {
  //      println("Unable to get text and entities for mention." + m)
  //      m
  //    }
  //
  //  }

  def annotationsFromFile(m: TacEntityMention, file: File) = {

    val xmlDoc = XML.loadFile(file)

    val entityLinks = xmlDoc \\ "entitylink"
    val linkedMentions = entityLinks.map(e => {
      //println((e \ "name").text)
      val mention = new SimpleEntityMention(m.docId, "", (e \ "mentionId").text, (e \ "name").text, "")
      val candidates = e \\ "candidate" take 100
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

  //    def extractNerNeighborhood(doc: Document) = {
  //      val neighbors = doc.attr[cc.factorie.app.nlp.mention.MentionList]
  //
  //      val namedMentions = neighbors.filter(m => {
  //        val mType = m.attr[MentionType].categoryValue
  //        (mType equals "NAM")
  //      })
  //
  //
  //      val allNers = namedMentions.map(m =>  {
  //        val eTypeAttr = m.attr[MentionEntityType]
  //        val eType = if (eTypeAttr != null) {
  //          eTypeAttr.categoryValue
  //        } else {
  //          "UNK"
  //        }
  //
  //        val charStart =   m.span.tokens.head.stringStart
  //        val charEnd = m.span.tokens.last.stringEnd
  //        val tokenStart = m.span.tokens.start
  //        val tokenEnd = tokenStart + m.span.tokens.length
  //
  //        new NlpXmlNerMention(m.span.string, Seq(), -1, false, tokenStart, tokenEnd, charStart, charEnd, eType)
  //      }
  //      )
  //
  //      allNers.toSeq
  //    }


  def featuresToProto(mention: TacEntityMention, candidates: Seq[ScoredWikipediaEntity]) {
    val basePath = outputDir + File.separator + mention.docId + "_" + mention.mentionId

    val svmFileName = basePath + "_m2eOnly.svm"
    val pbFile = basePath + "_m2eOnly.pbdat"

  //  val pw = new PrintWriter(svmFileName)

    val filename = pbFile + "_m2eOnly.pbdat"
    val file = new File(filename)
    println("Writing feature file: " + file.getAbsolutePath)

    val candsWithRank = candidates.zipWithIndex

    val output = new FileOutputStream(file)

    val tacLinkerFeatures = TacEntityMentionLinkerFeatures.newBuilder()
    tacLinkerFeatures.setNodeId(mention.nodeId)
    tacLinkerFeatures.setGroundTruthWikiTitle(mention.groundTruthWikiTitle.getOrElse(""))

    val mentionLinkerFeatures = EntityMentionLinkerFeatures.newBuilder()
    mentionLinkerFeatures.setEntityName(mention.entityName)
    mentionLinkerFeatures.setEntityType(mention.entityType)
    mentionLinkerFeatures.setMentionId(mention.mentionId)
    mentionLinkerFeatures.setSourceDocId(mention.docId)

    println("Extracting features...")
    for ((entity, rank) <- candsWithRank) {

      val galagoDoc = DocumentBridgeMap.getKbDocumentProvider.getDocument(entity.wikipediaTitle)
      entity.document = galagoDoc
      entity.incomingLinks = WikiLinkExtractor.simpleExtractorNoContext(galagoDoc).map(a => a.destination).toSet
      entity.outgoingLinks = galagoDoc.metadata.getOrElse("srcInlinks", "").split("\\s+").toSet
      entity.combinedLinks =  entity.incomingLinks ++ entity.outgoingLinks

      val m2eFeatures = Mention2EntityFeatureHasher.featuresAsMap(ConfInfo.rankingFeatures, mention, entity, candidates)
      val entityWithFeatures = ScoredWikipediaEntityFeatures.newBuilder()
      entityWithFeatures.setRank(rank + 1)

      entityWithFeatures.setWikipediaId(entity.wikipediaId)
      entityWithFeatures.setWikipediaTitle(entity.wikipediaTitle)
      entityWithFeatures.setScore(entity.score)

      for ((key, value) <- m2eFeatures) {
        val feature = LinkerFeature.newBuilder()
        feature.setKey(key)
        feature.setValue(value)
        entityWithFeatures.addRankingFeatures(feature)
      }
      mentionLinkerFeatures.addCandidates(entityWithFeatures)

      entity.document = null
  //    val svmFeatures = EntityFeaturesToSvmConverter.entityToSvmFormat(mention, entity, m2eFeatures)
     // pw.println(svmFeatures)

    }
    tacLinkerFeatures.setMention(mentionLinkerFeatures)
    tacLinkerFeatures.build().writeTo(output)



    if (true) {
      println(TextFormat.printToString(tacLinkerFeatures) + "\n")
    }
    output.close()
  //  pw.close()
  }

}

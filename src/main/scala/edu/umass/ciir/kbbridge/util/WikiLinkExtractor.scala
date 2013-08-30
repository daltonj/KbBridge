package edu.umass.ciir.kbbridge.util

import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.XML
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.HashMap
import org.lemurproject.galago.core.parse.Document
import edu.umass.ciir.kbbridge.data.ScoredWikipediaEntity
import edu.umass.ciir.kbbridge.search.DocumentBridgeMap

/**
 * Takes the input from the XML data and extractions the links from it.
 */
object WikiLinkExtractor {

  case class Anchor(source: String, destination: String, anchorText: String, paragraphId: Int, rawAnchorText: String);

  def extractLinks(document: Document): Seq[Anchor] = {

    val meta = document.metadata;
    var body = meta.get("xml");

    if (body != null) {
      body = body.replace("\\n", "\n")
    }

    val paragraphs =
      if (body == null || body == "\\N") NodeSeq.Empty
      else {
        try {
          val bodyXML = XML.loadString(body)
          (bodyXML \\ "paragraph") ++ (bodyXML \\ "list")
        } catch {
          case e: org.xml.sax.SAXParseException =>
            System.err.println("Article \"" + document.name + "\" has malformed XML in body:\n" + body, e.toString())
            NodeSeq.Empty
        }
      }

    // each paragraph maps into a tuple (wpid, title, AnchorArray[(String, String)]
    val links = paragraphs.zipWithIndex.map({
      case (paragraph, pIdx) => {
        val linksInParagraph = paragraph \\ "link"
        var outAnchors = linksInParagraph.map(link => extractLinkText(document.name, link, linksInParagraph, pIdx)).toArray
        outAnchors.filter(a => a.source != a.destination && a.destination.length() > 0 && a.anchorText.length() > 0).toSeq
      }
    }) // paragraphs.map returns a list of tuples per paragraph

    links.flatten.toSet.toSeq
  }

  def simpleExtractorNoContext(document: Document): Seq[Anchor] = {
    val body = document.metadata.get("xml")
    try {
      val bodyXML = XML.loadString(body.replace("\\n", "\n"))
      val links = bodyXML \\ "link"
      val outAnchors = links.map(link => extractAnchorFromLink(document.name, link)).filter(a => a.source != a.destination && a.destination.length() > 0 && a.anchorText.length() > 0)
      outAnchors
    } catch {
      case e: org.xml.sax.SAXParseException => extractLinks(document)
    }
  }

  def extractAnchorFromLink(src: String, n: Node) = {
    val destination = (n \ "target").text

    val p = (n \ "part").text
    val anchorText = if (p.length > 0) {
      p
    } else {
      destination
    }
    new Anchor(src, destination.replaceAll(" ", "_"), anchorText.replaceAll(",", "_"), -1, rawAnchorText = anchorText)
  }

  def extractLinkText(src: String, n: Node, context: scala.xml.NodeSeq, paragraphIdx: Int): Anchor = {

    var destination = (n \ "target").text
    var destinationTitle = destination.replaceAll(" ", "_")

    var contextPages = new ListBuffer[String]
    for (link <- context) {
      var target = (link \ "target").text.replaceAll(" ", "_")
      if (target.length() > 1 && !(destinationTitle equalsIgnoreCase target)) {
        contextPages += target
      }
    }

    // limit context to first 10 links
    contextPages = contextPages take 10

    var p = (n \ "part").text
    var anchorText = ""
    if (p.length > 0) {
      anchorText = p
    } else {
      anchorText = destination
    }
    new Anchor(src, destinationTitle, anchorText.replaceAll(",", "_"), paragraphIdx, rawAnchorText = anchorText)
  }


  def main(args: Array[String]) {

    val metadata = new HashMap[String, String];
    val testEntity = new ScoredWikipediaEntity("Amherst_College",
      5407,
      0.0d,
      1)
    val document = DocumentBridgeMap.getKbDocumentProvider.getDocument("Amherst_College")
    val links = simpleExtractorNoContext(document)
    for (a <- links) {
      println(a)
    }
  }

}
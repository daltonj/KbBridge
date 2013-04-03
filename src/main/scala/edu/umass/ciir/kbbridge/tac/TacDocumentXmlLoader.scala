package edu.umass.ciir.kbbridge.tac

import xml.XML
import java.io.{StringReader, FileInputStream}
import java.util.zip.GZIPInputStream
import edu.umass.ciir.kbbridge.nlp.TextNormalizer
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import edu.umass.ciir.models.StopWordList

/**
 * 
 */

object TacDocumentXmlLoader {
  val parser = XML.withSAXParser((new SAXFactoryImpl).newSAXParser())

  def buildTermVector(text:String):Seq[String] = {
    TextNormalizer.normalizeText(text).split(" ").filterNot(StopWordList.isStopWord(_))
  }


  /**
   * Use a sax parser that only fetches content from <p> nodes.
   */
  def contextViaSaxFromString(content:String):Seq[String] = {
    val contentStream = new StringReader(content)
    parser.synchronized{
      val ns = parser.load(contentStream)
      (ns \\ "p").flatMap { p =>
        val pText = p.text.replaceAll("\n", " ")
         TextNormalizer.normalizeText(pText).split(" ")
      }
    }
  }

  /**
   * Use a sax parser that only fetches content from <p> nodes.
   */
  def contextViaSax(fileLoc:String):Seq[String] = {
    val contentStream = new FileInputStream(fileLoc)
    parser.synchronized{
      val ns = parser.load(contentStream)
      (ns \\ "p").flatMap { p =>
        val pText = p.text.replaceAll("\n", " ")
         TextNormalizer.normalizeText(pText).split(" ")
      }
    }
  }

  def contextViaGzipSax(fileLoc:String):Seq[String] = {
    val contentStream = new GZIPInputStream(new FileInputStream(fileLoc+".gz"))
    parser.synchronized{
      val ns = parser.load(contentStream)
      (ns \\ "p").flatMap { p =>
        val pText = p.text.replaceAll("\n", " ")
         TextNormalizer.normalizeText(pText).split(" ")
      }
    }
  }

  def fullTextViaSax(fileLoc:String):String = {
    val contentStream = new FileInputStream(fileLoc)
    parser.synchronized{
      val ns = parser.load(contentStream)
      (ns \\ "p").map({ p =>
        val pText = p.text.replaceAll("\n", " ")
        pText
      }).mkString("")

    }
  }

  def fullTextViaGzipSax(fileLoc:String):String = {
    val contentStream = new GZIPInputStream(new FileInputStream(fileLoc+".gz"))
    parser.synchronized{
      val ns = parser.load(contentStream)
      (ns \\ "p").map({ p =>
        val pText = p.text.replaceAll("\n", " ")
        pText
      }).mkString("")

    }
  }

  /**
   * Use a picky sax parser
   */
  def contextViaSax2(fileLoc:String):Seq[String] = {
    val contentFile = io.Source.fromFile(fileLoc)
    val text = scala.xml.parsing.XhtmlParser.apply(contentFile).text
     TextNormalizer.normalizeText(text).split(" ")
  }


  def fullTextViaTacSource(fileLoc:String):String = {
    var doc1:TacSourceDocument = null
    val handler = new TacSourceHandler(){
      def foundDoc(doc: TacSourceDocument) {
        doc1 = doc
      }
    }
    val re = new TacSourceReader(handler)
    re.readSourceFile(fileLoc)

    doc1.getFulltext
  }

  def contextViaTacSource(fileLoc:String):Seq[String] = {
    val text = fullTextViaTacSource(fileLoc)
     TextNormalizer.normalizeText(text).split(" ")
  }


  /**
   * Load a plain text.
   */
  def contextPlain(fileLoc:String):Seq[String] = {
    val contentFile = io.Source.fromFile(fileLoc)
    contentFile.getLines().flatMap(l =>  TextNormalizer.normalizeText(l).split(" ")).toSeq
  }

  /**
   * Load a plain text.
   */
  def contextPlainFromString(content:String):Seq[String] = {
     TextNormalizer.normalizeText(content).split(" ")
  }

}
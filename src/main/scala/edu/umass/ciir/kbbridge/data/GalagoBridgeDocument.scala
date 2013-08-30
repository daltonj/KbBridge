package edu.umass.ciir.kbbridge.data

import edu.umass.ciir.kbbridge.search.DocumentBridgeMap
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.lemurproject.galago.core.parse.Document

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 5:23 PM
 */
trait GalagoBridgeDocument extends ScoredBridgeDocument {
  def documentname: String

  def rawScore: Option[Double]

  def relevanceScore: Option[Double]

  def rank: Option[Int]

  def galagoDocument: Option[org.lemurproject.galago.core.parse.Document]

  def passageInfo: Seq[(Int, Int)]

  /** Same thing just without the galago document (and therefore no access to text or metadata) */
  def dropDocument: GalagoBridgeDocument

  /** Same thing with the galago document and access to text or metadata */
  def ressurectDocument(docProvider: DocumentProvider): GalagoBridgeDocument
}

object GalagoDocumentTools {
  def fixGalagoDoc(galagoDoc: Document, faketokenize: (String) => Document){
    // we need use passage info, but termChar's are not set.
    // call fake tokenizer to fix this
    if (galagoDoc.termCharBegin == null || galagoDoc.termCharBegin.isEmpty) {
      val fakeDoc = faketokenize(galagoDoc.text)
      galagoDoc.termCharBegin = fakeDoc.termCharBegin
      galagoDoc.termCharEnd = fakeDoc.termCharEnd
    }
  }

  def passageMaskGalagoDoc(galagoDoc: Document, passageInfo: Seq[(Int, Int)]): Document = {
    def passagetext(galagoDocument: org.lemurproject.galago.core.parse.Document, passageInfo: Seq[(Int, Int)] = Seq.empty): String = {
      val t = galagoDocument.text
      if (t == null) ""
      else if (passageInfo.isEmpty) t
      else {
        (
          for ((begin, end) <- passageInfo) yield {
            val beginCharIndex = galagoDocument.termCharBegin.get(begin)
            val endCharIndex = galagoDocument.termCharEnd.get(end - 1)
            t.substring(beginCharIndex, endCharIndex)
          }
          ).mkString("\n")
      }
    }
    def passageterms(galagoDocument: org.lemurproject.galago.core.parse.Document, passageInfo: Seq[(Int, Int)] = Seq.empty): Seq[String] = {
      val t = galagoDocument.terms
      if (t == null) Seq.empty[String]
      else if (passageInfo.isEmpty) t
      else {
        (
          for ((begin, end) <- passageInfo) yield {
            t.subList(begin, end)
          }
          ).flatten
      }
    }
    def passagetags(galagoDocument: org.lemurproject.galago.core.parse.Document, passageInfo: Seq[(Int, Int)], termIndexMap: Map[Int, Int],
                    oldTermToBeginCharMap: Map[Int, Int], oldTermToEndCharMap: Map[Int, Int]): Seq[org.lemurproject.galago.core.parse.Tag] = {
      def overlaps(tagBegin: Int, tagEnd: Int, passageBegin: Int, passageEnd: Int): Boolean = {
        ((tagEnd > passageBegin) && (tagBegin < passageEnd))
      }
      val t = galagoDocument.tags
      if (t == null) Seq.empty[org.lemurproject.galago.core.parse.Tag]
      else if (passageInfo.isEmpty) t
      else {

        for (tag <- t; p <- passageInfo; if overlaps(tag.begin, tag.end, p._1, p._2)) yield {
          val cutTermBegin = if (tag.begin < p._1) p._1 else tag.begin
          val cutTermEnd = if (tag.end > p._2) p._2 else tag.end
          val termBegin = termIndexMap(cutTermBegin)
          val termEnd = termIndexMap(cutTermEnd - 1) + 1

          val charBegin = oldTermToBeginCharMap(cutTermBegin)
          val charEnd = oldTermToEndCharMap(cutTermEnd - 1) + 1
          new org.lemurproject.galago.core.parse.Tag(tag.name, tag.attributes, termBegin, termEnd, charBegin, charEnd)
        }
      }
    }



    val galagoPassageDoc =
      if (passageInfo.isEmpty) galagoDoc
      else {

        val pdoc = new org.lemurproject.galago.core.parse.Document(galagoDoc)
        val termIndexMap = (
                             passageInfo.flatMap(begend => begend._1 until begend._2)
                             ).zipWithIndex.toMap

        val charIndexMap = (
                             passageInfo.flatMap(begend => {
                               //              println("begend = "+begend )
                               val b = galagoDoc.termCharBegin(begend._1).toInt
                               val e = galagoDoc.termCharEnd(begend._2 - 1).toInt
                               b until e
                             })
                             ).zipWithIndex.toMap

        val oldTermToNewBeginChar =
          passageInfo.flatMap(begend => begend._1 until begend._2).map(termIdx => {
            val oldChar = galagoDoc.termCharBegin(termIdx)
            val newChar = charIndexMap(oldChar)
            termIdx -> newChar
          })

        val oldTermToNewEndChar =
          passageInfo.flatMap(begend => begend._1 until begend._2).map(termIdx => {
            val oldChar = galagoDoc.termCharEnd(termIdx)
            val newChar = charIndexMap(oldChar - 1) + 1
            termIdx -> newChar
          })


        pdoc.text = passagetext(galagoDoc, passageInfo)
        pdoc.terms = passageterms(galagoDoc, passageInfo)
        pdoc.tags = passagetags(galagoDoc, passageInfo, termIndexMap, oldTermToNewBeginChar.toMap,
                                oldTermToNewEndChar.toMap)
        pdoc.termCharBegin = seqAsJavaListConverter(oldTermToNewBeginChar.sortBy(_._1).map(_._2).map(new java.lang.Integer(_))).asJava
        pdoc.termCharEnd = seqAsJavaListConverter(oldTermToNewEndChar.sortBy(_._1).map(_._2).map(new java.lang.Integer(_))).asJava
        pdoc
      }
    galagoPassageDoc
  }

}

class GalagoBridgeDocumentWrapper(val documentname: String, val rawScore: Option[Double] = None, val relevanceScore: Option[Double] = None, val rank: Option[Int] = None, val galagoDocument: Option[org.lemurproject.galago.core.parse.Document], val passageInfo: Seq[(Int, Int)] = Seq.empty) extends ScoredBridgeDocument with GalagoBridgeDocument {
  import GalagoDocumentTools._

  def metadata: Map[String, String] = {
    if (galagoDocument.isEmpty) Map.empty
    else {
      val m = galagoDocument.get.metadata
      if (m == null) Map.empty
      else m.toMap
    }
  }

  def text: String = {
    if (galagoDocument.isEmpty) ""
    else {
      val t = galagoDocument.get.text
      if (t == null) ""
      else t
    }
  }

  def terms: Seq[String] = {
    if (galagoDocument.isEmpty) Seq.empty[String]
    else {
      val t = galagoDocument.get.terms
      if (t == null) Seq.empty[String]
      else t
    }
  }

  def dropDocument: GalagoBridgeDocument = {
    if (galagoDocument.isEmpty) this.asInstanceOf[GalagoBridgeDocument]
    else new GalagoBridgeDocumentWrapper(documentname, rawScore, relevanceScore, rank, None).asInstanceOf[GalagoBridgeDocument]
  }

  def ressurectDocument(docProvider: DocumentProvider): GalagoBridgeDocument = {
    if (galagoDocument.isDefined) this.asInstanceOf[GalagoBridgeDocument]
    else {
      val galagoDoc = docProvider.getDocument(documentname)


      fixGalagoDoc(galagoDoc, faketokenize=docProvider.fakeTokenize)
      val galagoPassageDoc: Document = passageMaskGalagoDoc(galagoDoc, passageInfo)

      new GalagoBridgeDocumentWrapper(documentname, rawScore, relevanceScore, rank, Some(galagoPassageDoc)).asInstanceOf[GalagoBridgeDocument]
    }
  }
}

object GalagoBridgeDocumentMain {
  def main(args: Array[String]) {

    {
      val bd = new GalagoBridgeDocumentWrapper("Andre_Agassi", galagoDocument = None)
      val bdd = bd.ressurectDocument(DocumentBridgeMap.getKbDocumentProvider)
      val terms = bdd.galagoDocument.get.terms
      val allTerms = bdd.galagoDocument.get.terms.mkString(" ").substring(0, 1000)
      val allTags = bdd.galagoDocument.get.tags.map(t => t.toString + "_" + terms.subList(t.begin, t.end).mkString("_")).mkString(" ").substring(0, 1000)
      val allText = bdd.galagoDocument.get.text.substring(bdd.galagoDocument.get.text.length() - 100)

      println("terms  = \n" + allTerms)
      println("tags  = \n" + allTags)
      println("text = \n" + allText)


      println("allTags.length = " + bdd.galagoDocument.get.tags.length)
      println("allTerms.length = " + bdd.galagoDocument.get.terms.length)
    }
    println("==================\n\n\n\n===============\n")


    {
      val bd = new GalagoBridgeDocumentWrapper("Andre_Agassi", galagoDocument = None, passageInfo = Seq((120, 200)))
      val bdd = bd.ressurectDocument(DocumentBridgeMap.getKbDocumentProvider)
      val terms = bdd.galagoDocument.get.terms
      val allTerms = bdd.galagoDocument.get.terms.mkString(" ").take(1000)
      val allTags = bdd.galagoDocument.get.tags.map(t => t.toString).mkString(" ").take(1000)
      //      val allTags = bdd.galagoDocument.get.tags.map(t => t.toString+"_"+terms.subList(t.begin, t.end).mkString("_")).mkString(" ").substring(0,1000)
      val allText = bdd.galagoDocument.get.text.substring(bdd.galagoDocument.get.text.length() - 100)

      println("terms  = \n" + allTerms)
      println("tags  = \n" + allTags)
      println("text = \n" + allText)

      println("termCharsBegin = "+bdd.galagoDocument.get.termCharBegin)
      println("termCharsEnd = "+bdd.galagoDocument.get.termCharEnd)

      val tt = bdd.galagoDocument.get.text

      val strings =
      for((chb, che) <- bdd.galagoDocument.get.termCharBegin zip bdd.galagoDocument.get.termCharEnd) yield {
        tt.substring(chb, che)
      }

      println(strings.mkString(" "))


    }


  }
}


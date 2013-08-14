//package edu.umass.ciir.kbbridge.nlp
//
//import java.io.{BufferedWriter, FileWriter, File}
//import java.util.Date
//import edu.umass.ciir.kbbridge.util.ConfInfo
//
///**
// *
// */
//
//object NlpExtractor {
//
//
//  def getTextFrom(fResult:File) : String = {
//    val source = io.Source.fromFile(fResult,"ISO-8859-1") // UTF-8 did not work
//    val result = source.getLines().mkString("\n")
//    source.close()
//    result
//
//  }
//
//
//  def getSafeFileName(docid: String, source: String): String = {
//    /**
//     * replaces all slashes with "--"
//     */
//    val docidSafe1 = docid.replaceAll("/", "--")
//
//    /**
//     * Check whether the filename contains "%23" and convert it to "-hex-23"
//     */
//    val docidSafe2 =
//      (for (shingle <- (docidSafe1 + "  ").sliding(3)) yield {
//        if (shingle(0) == '%' && shingle(1).isLetterOrDigit && shingle(2).isLetterOrDigit) {
//          "-hex-"
//        } else {
//          shingle(0).toString
//        }
//      }).mkString("")
//
//    /**
//     * replace all non-ASCII characters with "-hex-<codepoint>"
//     */
//    val docidSafe3 =
//      (for ((char, i) <- docidSafe2.zipWithIndex) yield {
//        if (docidSafe2.codePointAt(i) < 128) {
//          char.toString
//        } else {
//          "-hex" + char.toInt.toHexString + "-"
//        }
//      }).mkString("")
//
//    /**
//     * If the filename starts with ./- prepend a Z to it
//     */
//    val docidSafe4 =
//      if (docidSafe3.startsWith(".") || docidSafe3.startsWith("-")) {
//        "Z" + docidSafe3
//      } else {
//        docidSafe3
//      }
//
//    val suffLen = ConfInfo.nlpExtractPathStanford.length + source.length + 1 + ".xml".length
//
//    /**
//     * Making sure the filename is less than 255 charcters long
//     */
//    val docidSafe =
//      if ((docidSafe4.length + suffLen) < 255) {
//        docidSafe4
//      } else {
//        docidSafe4.substring(docidSafe4.length - 255 + suffLen)
//      }
//    docidSafe
//  }
//
//  def getOrQueueNlp(fullText:()=>String,  docid:String, source:String):Option[File] = {
//    val docidSafe: String = getSafeFileName(docid, source)
//
//    val fResult = new File(ConfInfo.nlpExtractPathStanford+source+"/"+docidSafe+".xml")
//
//    if(!fResult.exists()){
//      None
//    } else {
//      Some(fResult)
//     }
//  }
//
//
//  def getOrQueueNlpContent(fullText:()=>String,  docid:String, source:String):Option[String] = {
//    getOrQueueNlp(fullText, docid,source) match {
//      case Some(fResult) => Some(getTextFrom(fResult))
//      case None => None
//    }
//  }
//
//
//}
//
//case class NeedExtractionException(filename:String) extends Exception {}
//case class WaitForExtractionException(filename:String, lastModified:Date) extends Exception{}
//
//
////object NlpExtractorApp {
////
////  def main(args:Array[String]) {
//////    val ex = new NlpStanfordExtractor(false)
//////
////      val text1 = "Christian Borle, an actor in NBC’s Smash, is also keeping a foot in the theater, playing a pirate in Peter and the Starcatcher, opening Sunday on Broadway."
////      val text2 = "Stanford CoreNLP provides a set of natural language analysis tools which can take raw English language text input and give the base forms of words, their parts of speech, whether they are names of companies, people, etc., normalize dates, times, and numeric quantities, and mark up the structure of sentences in terms of phrases and word dependencies, and indicate which noun phrases refer to the same entities. It provides the foundational building blocks for higher level text understanding applications."
//////    val mongoDoc = ex.parseFile("Christian Borle, an actor in NBC’s Smash, is also keeping a foot in the theater, playing a pirate in Peter and the Starcatcher, opening Sunday on Broadway.")
//////    println(mongoDoc)
////
////    val ex = NlpExtractor
////    val r1 = ex.getOrQueueNlpContent(()=>{text1},"Würtzburg","tmp")
////    val r2 = ex.getOrQueueNlpContent(()=>{text2},"中華民族","tmp")
////
////    println("r1 = "+r1)
////    println("r2 = "+r2)
////
////  }
////}
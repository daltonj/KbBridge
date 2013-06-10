package edu.umass.ciir.kbbridge.nlp

import java.io.{BufferedWriter, FileWriter, File}
import java.util.Date
import edu.umass.ciir.kbbridge.util.ConfInfo

/**
 * 
 */

object NlpExtractor {
  def tagFile(fRaw: File) {
    val w2 = new BufferedWriter(new FileWriter(ConfInfo.nlpExtractListStanford, true))
    w2.write(fRaw.getAbsolutePath + "\n")
    w2.close()
  }

  def getDirectoryOfFile(fRaw:File):String = {
    val pathWithFile = fRaw.getAbsolutePath
    val pathNoFile = pathWithFile.substring(0,pathWithFile.lastIndexOf(File.separator))
    pathNoFile
  }

  def exportRawtext(fRaw: File, fullText: String)  {
    (new File(getDirectoryOfFile(fRaw))).mkdirs()
    // cant find raw file - export!
    val w = new BufferedWriter(new FileWriter(fRaw, false))
    w.write(fullText)
    w.close()

    tagFile(fRaw)
  }

  def getTextFrom(fResult:File) : String = {
    val source = io.Source.fromFile(fResult,"ISO-8859-1") // UTF-8 did not work
    val result = source.getLines().mkString("\n")
    source.close()
    result

  }


  def getSafeFileName(docid: String, source: String): String = {
    /**
     * replaces all slashes with "--"
     */
    val docidSafe1 = docid.replaceAll("/", "--")

    /**
     * Check whether the filename contains "%23" and convert it to "-hex-23"
     */
    val docidSafe2 =
      (for (shingle <- (docidSafe1 + "  ").sliding(3)) yield {
        if (shingle(0) == '%' && shingle(1).isLetterOrDigit && shingle(2).isLetterOrDigit) {
          "-hex-"
        } else {
          shingle(0).toString
        }
      }).mkString("")

    /**
     * replace all non-ASCII characters with "-hex-<codepoint>"
     */
    val docidSafe3 =
      (for ((char, i) <- docidSafe2.zipWithIndex) yield {
        if (docidSafe2.codePointAt(i) < 128) {
          char.toString
        } else {
          "-hex" + char.toInt.toHexString + "-"
        }
      }).mkString("")

    /**
     * If the filename starts with ./- prepend a Z to it
     */
    val docidSafe4 =
      if (docidSafe3.startsWith(".") || docidSafe3.startsWith("-")) {
        "Z" + docidSafe3
      } else {
        docidSafe3
      }

    val suffLen = ConfInfo.nlpExtractPathStanford.length + source.length + 1 + ".xml".length

    /**
     * Making sure the filename is less than 255 charcters long
     */
    val docidSafe =
      if ((docidSafe4.length + suffLen) < 255) {
        docidSafe4
      } else {
        docidSafe4.substring(docidSafe4.length - 255 + suffLen)
      }
    docidSafe
  }

  def getOrQueueNlp(fullText:()=>String,  docid:String, source:String):Option[File] = {
    val docidSafe: String = getSafeFileName(docid, source)

    val fResult = new File(ConfInfo.nlpExtractPathStanford+source+"/"+docidSafe+".xml")

    if(!fResult.exists()){
        if(ConfInfo.createNlpInput){
          //System.err.println("Can't find NLP ressource at "+fResult.getAbsolutePath)
          // cant find result
          val fRaw = new File(ConfInfo.nlpExtractOutputPathStanford+source+"/"+docidSafe)
          if(!fRaw.exists()){
            exportRawtext(fRaw, fullText())
  //          System.err.println("Stanford NLP Extraction needed for "+fRaw.getAbsolutePath)
            None
          } else {
            // wait for the extractopm to finish
            tagFile(fRaw)

  //          System.err.println("waiting for Stanford NLP Extraction to finish for "+fRaw.getAbsolutePath+" (created at "+new Date(fRaw.lastModified()).toLocaleString+")")
            None
          }
        } else None
    } else {
      Some(fResult)
     }
  }


  def getOrQueueNlpContent(fullText:()=>String,  docid:String, source:String):Option[String] = {
    getOrQueueNlp(fullText, docid,source) match {
      case Some(fResult) => Some(getTextFrom(fResult))
      case None => None
    }
  }



  def createExtractScript(){
    val scriptFile = new File(ConfInfo.nlpExtractScriptStanford)
    if(!scriptFile.exists()){
      val listFile = ConfInfo.nlpExtractListStanford
      val listFileS = ConfInfo.nlpExtractListStanford+"-"
      val listFileU = ConfInfo.nlpExtractListStanford+"u"
      val cmd = "java -cp stanford-corenlp-2012-04-09.jar:stanford-corenlp-2012-04-09-models.jar:xom.jar:joda-time.jar -Xmx10g edu.stanford.nlp.pipeline.StanfordCoreNLP -filelist "+listFile+" -outputDirectory "+ConfInfo.nlpExtractPathStanford+" -noClobber -props StanfordCoreNLP.properties"
      val cmdBatch = "java -cp stanford-corenlp-2012-04-09.jar:stanford-corenlp-2012-04-09-models.jar:xom.jar:joda-time.jar -Xmx10g edu.stanford.nlp.pipeline.StanfordCoreNLP -filelist $f -outputDirectory "+ConfInfo.nlpExtractPathStanford+" -noClobber -props StanfordCoreNLP.properties"
      val w = new FileWriter(scriptFile)
      w.write("#!/bin/sh\n\n")

      w.write("rm "+listFileS+"*\n")
      w.write("rm "+listFileU+"\n")
      w.write("sort "+listFile+" | uniq > "+listFileU+"\n")
      w.write("split -l 30 "+listFileU+" "+listFileS +"\n")
      w.write("cd "+ConfInfo.nlpExtractExecStanford+"\n")
      w.write("for f in `ls "+listFileS+"*`; do "+"\n")
      w.write("("+"\n")
      w.write(""+cmdBatch+" &> $f.log"+"\n")
      w.write("rm $f"+"\n")
      w.write(") &"+"\n")
      w.write("done"+"\n")

      w.write("#rm "+listFile+"\n")
      w.close()
    }
  }
}

case class NeedExtractionException(filename:String) extends Exception {}
case class WaitForExtractionException(filename:String, lastModified:Date) extends Exception{}


object NlpExtractorApp {

  def main(args:Array[String]) {
//    val ex = new NlpStanfordExtractor(false)
//
      val text1 = "Christian Borle, an actor in NBC’s Smash, is also keeping a foot in the theater, playing a pirate in Peter and the Starcatcher, opening Sunday on Broadway."
      val text2 = "Stanford CoreNLP provides a set of natural language analysis tools which can take raw English language text input and give the base forms of words, their parts of speech, whether they are names of companies, people, etc., normalize dates, times, and numeric quantities, and mark up the structure of sentences in terms of phrases and word dependencies, and indicate which noun phrases refer to the same entities. It provides the foundational building blocks for higher level text understanding applications."
//    val mongoDoc = ex.parseFile("Christian Borle, an actor in NBC’s Smash, is also keeping a foot in the theater, playing a pirate in Peter and the Starcatcher, opening Sunday on Broadway.")
//    println(mongoDoc)

    val ex = NlpExtractor
    val r1 = ex.getOrQueueNlpContent(()=>{text1},"Würtzburg","tmp")
    val r2 = ex.getOrQueueNlpContent(()=>{text2},"中華民族","tmp")

    println("r1 = "+r1)
    println("r2 = "+r2)

  }
}
package edu.umass.ciir.kbbridge.nlp

import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlMention


/**
 * 
 */

object NerExtractor {
//  lazy val lbj = new NerLbjExtractor()
//  lazy val nerEx = new NerStanfordExtractor()

  //==================================
  //== Stanford CoreNLP ==
  //--
  //-

//
//  def extractStanfordNerSpans(fullText:String, docid:String="?"):Set[String] = {
//    val mNers:Seq[TokenMentionWithContext] = nerEx.stanfordPipeline(fullText,docid)
//    val mSet = mNers.map(m => m.getText).toSet
//    mSet
//  }
//  def getStanfordNerForMention(fullText:String, name:String, docid:String, normalizeName:(String)=>String = (x => x)):String = {
//    def findMention(ners:Seq[TokenMentionWithContext], name:String): Option[TokenMentionWithContext] = {
//      val opt1 = ners.find(m => m.getText equals name)
//      if(opt1.isDefined){
//        opt1
//      } else {
//        val opt2 = ners.find(m => m.getText equalsIgnoreCase name)
//        if(opt2.isDefined){
//          opt2
//        } else {
//          val opt3 = ners.find(m => normalizeName(m.getText) equalsIgnoreCase normalizeName(name))
//          opt3
//        }
//      }
//    }
//    val mNers:Seq[TokenMentionWithContext] = nerEx.stanfordPipeline(fullText,docid)
//    val mMention = findMention(mNers, name)
//    if (mMention.isEmpty) { "UNK"} else {
//      mMention.get.getNlpInfo.getNerType
//    }
//
//  }
//

  //==================================
  //== Lbj ==
  //--
  //-

//  val nerMemoMap = new HashMap[String,Seq[(String, String)]]()
//
//  def extractNerSpansWithTypeLbj(fullText:String,  docid:String="?"):Seq[(String, String)] = {
//    nerMemoMap.getOrElseUpdate(docid, {createExtractNerSpansWithTypeLbj(fullText, docid)})
//  }
//  def createExtractNerSpansWithTypeLbj(fullText:String,  docid:String="?"):Seq[(String, String)] = {
//    lbj.lbjNers(fullText, docid).map(pair => {
//      val res = pair.split("\t")
//      (res.head, res.tail.mkString("\t"))
//    })
//  }
//
//  def extractLbjNerSpans(fullText:String,  docid:String="?"):Set[String] = {
//    val nerwithtype = extractNerSpansWithTypeLbj(fullText,docid)
//    nerwithtype.map(_._2).toSet
//  }


  /**
   *  Determine the NER type of the name from the text.
   *
   *  gets all NERs from the fullText, searches for named entities that equal name,
   *  first with capitalization, then via equalsIgnoreCase,
   *  then by comparing normalized forms.
   */
  def findNerForName(ners: scala.Seq[(String, String)], name: String, normalizeName: (String) => String): String = {
    val ner = {
      val opt1 = ners.find(m => m._2 equals name)
      if (opt1.isDefined) {
        //        println("# "+name+" / "+opt1)
        opt1
      } else {
        val opt2 = ners.find(m => m._2 equalsIgnoreCase name)
        if (opt2.isDefined) {
          //          println("* "+name+" / "+opt2)
          opt2
        } else {
          val opt1a = ners.find(m => m._2 equalsIgnoreCase name.replaceAll("_", " "))
          if (opt1a.isDefined) {
            //            println("_ "+name+" / "+opt1a)
            opt1a
          } else {

            val opt3 = ners.find(m => normalizeName(m._2) equalsIgnoreCase normalizeName(name))
            //           if(opt3.isDefined) println(". "+name+" / "+opt3) else println("  "+name+" / "+opt3)
            opt3
          }
        }
      }
    }

    if (!ner.isDefined) {
      //        println("? "+name+" /  -- ")
      "UNK"
    } else {
      ner.get._1
    }
  }
//
//  def getLbjNerForMentionFromFullText(fullText: String, docid: String, name: String, normalizeName: (String) => String): String = {
//    val ners = extractNerSpansWithTypeLbj(fullText, docid)
//    findNerForName(ners, name, normalizeName)
//  }
//
//  /**
//   * Determine the NER type of the name, from a sentence of fulltext.
//   *
//   * Gets the sentence of fullText surrounding name (determined by equalsIgnoreCase), run NER detection, if a named entity with the same surface form of name is detected, its type is returned.
//   * If this is not the case, "UNK" is returned.
//   *
//   * quicker than {@link getLbjNerForMentionFromFullText}
//   *
//   */
//  def getLbjNerForMentionSentence(fullText:String, name:String, docid:String, normalizeName:(String=>String)):String = {
//    val index = fullText.toLowerCase.indexOf(name.toLowerCase)
//    if(index < 0 ) "UNK" else {
//      val beginSentence = math.max(0,fullText.lastIndexOf(".", index))
//      val endSentenceNF = fullText.indexOf(".", index)
//      val endSentence = if(endSentenceNF > 0) endSentenceNF else fullText.length() // in case no period was found.
//      val sentence = fullText.substring(beginSentence +1, endSentence)
//
//      val ners = extractNerSpansWithTypeLbj(sentence, sentence)
//      val ner = ners.find(m => m._2 equalsIgnoreCase name)
//      if (ner.isEmpty) {
//        "UNK"
//      } else {
//        ner.get._1
//      }
//
//    }
//
//  }
//
//  def getLbjNerForMention(fullText:String, name:String, docid:String, normalizeName:(String=>String)):String = {
//    getLbjNerForMentionFromFullText(fullText, docid, name, normalizeName)
//  }


  //==================================
  //== Perparsed Stanford ==
  //--
  //-

  def extractXmlNerSpans(fullText:() => String, docid:String, source:String):Set[String] = {
    NlpExtractor.getOrQueueNlp(fullText, docid,source) match {
      case Some(file) => {
        val ners = NlpExtractReader.getNers(file)
        ners.map(_._2).toSet
      }
      case None => {
        Set()
      }
    }
  }

    def getXmlNerForMentionFromFullText(fullText: () => String, docid: String, name: String, normalizeName: (String) => String, source:String): String = {
      NlpExtractor.getOrQueueNlp(fullText, docid, source) match {
        case Some(file) => {
          val ners = NlpExtractReader.getNers(file)
          findNerForName(ners, name, normalizeName)
        }
        case None => {
          "UNK"
        }
      }

    }

    def getXmlCorefForMentionFromFullText(fullText: () => String, docid: String, source:String): Seq[Seq[NlpXmlMention]] = {
      NlpExtractor.getOrQueueNlp(fullText, docid, source) match {
        case Some(file) => {
          val corefChains = CorefXmlReader.getCorefChains(file)
          corefChains
        }
        case None => {
          Seq()
        }
      }

    }


  //==================================
  //== Bindings ==
  //--
  //-

  def isPerLocOrg(ner:String):Boolean = {
    (ner equalsIgnoreCase "per") || (ner equalsIgnoreCase "org") || (ner equalsIgnoreCase "loc") || (ner equalsIgnoreCase "gpe")
  }

  def isLoc(ner:String):Boolean = {
    (ner equalsIgnoreCase "loc") || (ner equalsIgnoreCase "gpe")
  }


  def extractNerSpans(fullText:() => String, docid:String="?", source:String):Set[String] = {
//    extractStanfordNerSpans(fullText, docid)
//    extractLbjNerSpans(fullText, docid)
    extractXmlNerSpans(fullText, docid,source)
  }

  def getNerForMention(fullText:() => String, name:String, docid:String, normalizeName:(String => String), source:String):String = {
//    getStanfordNerForMention(fullText, docid = docid,name = name, normalizeName)
//    getLbjNerForMention(fullText, docid = docid,name = name, normalizeName)
    getXmlNerForMentionFromFullText(fullText, docid = docid,name = name, normalizeName, source = source)
  }

  def main(args:Array[String]) {
//    val ners = extractNerSpans("Christian Borle, an actor in NBC’s “Smash,” is also keeping a foot in the theater, playing a pirate in “Peter and the Starcatcher,” opening Sunday on Broadway.", "tmp")
    val fullText = "The Bachelor Sneak Peek: Rozlyn Papa Confronted on \"Women Tell All\" Special\n \n\n\n\nJake Pavelka comes back to face the women, Rozlyn Papa talks about the bachelor scandal, Michelle explains her crazy behavior, and Ali Fedotowsky apologizes to Vienna.\nIf you haven't seen the The Bachelor Women Tell All sneak peek, here it is!\n\nIf you can't remember who the contestants even were, see a re-cap from here.\n"
    val ners = extractNerSpans(() => {fullText}, "eng-WL-11-174584-12960463", "tmp")
    println(ners.mkString("\n"))

    val t = getNerForMention(() => {fullText}, "British Columbia Valishia", "eng-WL-11-174584-12960463", u => u.toLowerCase, "tmp")
    println("type = "+t)


  }
}
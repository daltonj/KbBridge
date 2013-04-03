package edu.umass.ciir.kbbridge.nlp

/**
 * 
 */

object NlpData {
  case class Token(rawWord:String,  ner:String, pos:String, lemma:String, charBegin:Int,  charEnd:Int, sentence:Int=(-1)) {
    def word : String = {
      rawWord match {
        case "-LRB-" => "("
        case "-RRB-" => ")"
        case "-RSB-" => "]"
        case "-LSB-" => "["
        case "-LCB-" => "{"
        case "-RCB-" => "}"
        case x => x
      }
    }
  }

  class NlpXmlMention(val text:String,  val tokens:Seq[Token], val sentenceId:Int, val isRepresentative:Boolean = false, val tokenStart:Int, val tokenEnd:Int,  val charBegin:Int, val charEnd:Int)
  class NlpXmlNerMention(override val text:String,  override val tokens:Seq[Token], override val sentenceId:Int, override val isRepresentative:Boolean = false, override  val tokenStart:Int, override val tokenEnd:Int,  override val charBegin:Int, override val charEnd:Int, val ner:String) extends NlpXmlMention(text, tokens, sentenceId, isRepresentative, tokenStart, tokenEnd, charBegin, charEnd)


  def xmlMention2NerMention(nerType:String,  mention:NlpXmlMention):NlpXmlNerMention = {
    new NlpXmlNerMention(mention.text, mention.tokens, mention.sentenceId, mention.isRepresentative, mention.tokenStart, mention.tokenEnd, mention.charBegin, mention.charEnd, nerType)
  }

}
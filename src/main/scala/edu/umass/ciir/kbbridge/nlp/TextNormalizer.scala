package edu.umass.ciir.kbbridge.nlp


/**
 * 
 */

object TextNormalizer {

  def normalizeText(name:String):String = {
    val lower = name.toLowerCase
    val symbolsToSpace = lower.replaceAll("\\s", " ").replaceAll("[^a-z01-9 ]", " ")
    val zappedSpaces = symbolsToSpace.split(" ").filter(!_.isEmpty).mkString(" ")
    zappedSpaces.trim()
  }

}
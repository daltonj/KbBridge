package edu.umass.ciir.kbbridge.features.util

import edu.umass.ciir.models.StopWordList
import edu.umass.ciir.kbbridge.nlp.TextNormalizer

object StringSimilarity {
  
  
  def diceCharacter(s1: String, s2:String) : Double = {
    val s1CharSet = s1.toCharArray.toSet
    val s2CharSet = s2.toCharArray.toSet
    val diceCharacter = SetMeasures(s1CharSet, s2CharSet).diceCorrect
    diceCharacter
  }
  
  def diceWords(s1: String, s2:String) : Double = {
    val s1WordSet = s1.split(" ").toSet
    val s2WordSet = s2.split(" ").toSet
    val diceWord = SetMeasures(s1WordSet, s2WordSet).diceCorrect
    diceWord
  }
  
  def firstCharacterMatch(s1: String, s2:String) : Double = {
    if (s1.length > 0 && s2.length > 0) {
      if (s1(0) == s2(0)) {
        1.0
      } else {
       0.0
      }
    } else {
      0.0
    }
  }
  
  
  /**
   * Tests whether s2 is possible acronym expansion for s1
   * Return 1.0 if s2 is an acronym expansion, 0.0 if it is not.
   * 
   */
  def acronymTest(s1:String, s2:String) : Double = {

    val words = s2.split("\\s+").toSeq
    val firstCharacters = words.withFilter(w => !StopWordList.isStopWord(w.toLowerCase)).map(w => w(0)).mkString
    
    if (TextNormalizer.normalizeText(s1) equals TextNormalizer.normalizeText(firstCharacters)) {
      1.0
    } else {
      0.0
    }
  }
  
  /**
   * Tests whether s1 starts with s2
   */
  def exactMatch(s1:String, s2:String) : Double = {
     if (s1 equals s2) {
       1.0
     } else {
       0.0
     }
  }
  
  /**
   * Tests whether s1 starts with s2
   */
  def prefixOf(s1:String, s2:String) : Double = {
     if (s1.length() > 0 && s1.startsWith(s2)) {
       1.0
     } else {
       0.0
     }
  }
  
  def suffixOf(s1:String, s2:String) : Double = {
     if (s1.length() > 0 && s1.endsWith(s2)) {
       1.0
     } else {
       0.0
     }
  }
  
}
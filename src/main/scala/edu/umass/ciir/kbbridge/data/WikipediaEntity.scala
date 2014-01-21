package edu.umass.ciir.kbbridge.data

import org.lemurproject.galago.core.parse.Document

/**
 * User: jdalton
 * Date: 3/29/13
 */
case class WikipediaEntity (wikipediaTitle:String, wikipediaId:Int, var document : Document = null, var incomingLinks : Set[String] = Set(), var outgoingLinks : Set[String] = Set(), var combinedLinks : Set[String] = Set())   {
  def name:String = wikipediaTitle.replaceAll("_", " ")
}

class ScoredWikipediaEntity(override val wikipediaTitle:String,
                            override val wikipediaId:Int,
                            var score:Double,
                            var rank:Int,
                            val featureMap:Option[Map[String,Double]]=None)
  extends WikipediaEntity (wikipediaTitle,wikipediaId)

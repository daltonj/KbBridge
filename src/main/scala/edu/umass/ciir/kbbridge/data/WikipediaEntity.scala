package edu.umass.ciir.kbbridge.data

import org.lemurproject.galago.core.parse.Document

/**
 * User: jdalton
 * Date: 3/29/13
 */
case class WikipediaEntity (wikipediaTitle:String, wikipediaId:Int, metadata: Map[String,String], var document : Document = null)   {
  def name:String = wikipediaTitle.replaceAll("_", " ")
}

class ScoredWikipediaEntity(override val wikipediaTitle:String, override val wikipediaId:Int,
                                 override val metadata: Map[String,String], val score:Double, var rank:Int, val featureMap:Option[Map[String,Double]]=None)
  extends WikipediaEntity (wikipediaTitle,wikipediaId, metadata)

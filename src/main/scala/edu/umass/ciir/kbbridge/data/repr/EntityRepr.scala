package edu.umass.ciir.kbbridge.data.repr


/**
 * User: dietz
 * Date: 6/7/13
 * Time: 2:42 PM
 */
case class EntityRepr(entityName:String, queryId:Option[String]=None, nameVariants:Iterable[(String, Double)]=Seq.empty, neighbors:Iterable[(EntityRepr, Double)] = Seq.empty, words:Iterable[(String, Double)]=Seq.empty, wikipediaTitleInput:Option[String] = None){
  val wikipediaTitle:String = {
    wikipediaTitleInput match {
      case Some(title) => title
      case None => entityName.replaceAllLiterally(" ","_")
    }
  }

  /** same thing, just without neighbors. This is needed for unroll/flatten operations */
  def dropNeighbors:EntityRepr = {
    EntityRepr(entityName, queryId, nameVariants, Seq.empty, words, wikipediaTitleInput)
  }
}


package edu.umass.ciir.kbbridge.data.repr


/**
 * User: dietz
 * Date: 6/7/13
 * Time: 2:42 PM
 */
case class EntityRepr(entityName:String, queryId:Option[String]=None, nameVariants:Seq[(String, Double)]=Seq.empty, neighbors:Seq[(EntityRepr, Double)] = Seq.empty, words:Seq[(String, Double)]=Seq.empty)


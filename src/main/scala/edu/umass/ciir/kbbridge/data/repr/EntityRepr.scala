package edu.umass.ciir.kbbridge.data.repr


/**
 * User: dietz
 * Date: 6/7/13
 * Time: 2:42 PM
 */
case class EntityRepr(entityName:String, queryId:Option[String]=None, nameVariants:Iterable[(String, Double)]=Seq.empty, neighbors:Iterable[(EntityRepr, Double)] = Seq.empty, words:Iterable[(String, Double)]=Seq.empty)


package edu.umass.ciir.kbbridge.data

import collection.mutable.HashMap
import edu.umass.ciir.kbbridge.util.ConfInfo

/**
 * 
 */

object IdMap {

  val wikiId2tacIdMap = {
    if (ConfInfo.useTacIdMap) TacId2WikiTitleMap.wikiId2tacIdMap
    else Map.empty[Int,String].withDefaultValue("")
  }
  val tacId2wikiIdMap = {
    if (ConfInfo.useTacIdMap) TacId2WikiTitleMap.tacId2wikiIdMap
    else Map.empty[String,Int].withDefaultValue(1)
  }
  val tacId2tacTypeMap = {
    if (ConfInfo.useTacIdMap) TacId2WikiTitleMap.tacId2tacTypeMap
    else Map.empty[String,String].withDefaultValue("")
  }

  val tacId2WikiTitleMap = {
    if (ConfInfo.useTacIdMap) TacId2WikiTitleMap.tacId2WikiTitleMap
    else Map.empty[String,String].withDefaultValue("")
  }
  val tacId2TacTitleMap = {
    if (ConfInfo.useTacIdMap) TacId2WikiTitleMap.tacId2TacTitleMap
    else Map.empty[String,String].withDefaultValue("")
  }
  val wikiTitle2TacIdMap = {
    if (ConfInfo.useTacIdMap) TacId2WikiTitleMap.wikiTitle2TacIdMap
    else Map.empty[String,String].withDefaultValue("")
  }
  val wikiTitle2WikiIdMap = {
    if (ConfInfo.useTacIdMap) TacId2WikiTitleMap.wikiTitle2WikiIdMap
    else Map.empty[String,Int].withDefaultValue(1)
  }
}
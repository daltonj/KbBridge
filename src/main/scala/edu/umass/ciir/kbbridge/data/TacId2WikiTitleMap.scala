package edu.umass.ciir.kbbridge.data

import collection.mutable.HashMap
import edu.umass.ciir.kbbridge.util.ConfInfo

/**
 *
 */

object TacId2WikiTitleMap {
  private val f = io.Source.fromFile(ConfInfo.idmap)
  private val wikiId2TacId = new HashMap[Int,  String]()
  private val tacId2wikiId = new HashMap[ String, Int]()
  private val tacId2tacType = new HashMap[String,  String]()
  private val wikiTitle2TacId = new HashMap[String,  String]()
  private val wikiTitle2WikiId = new HashMap[String,  Int]()
  private val tacId2WikiTitle = new HashMap[String,  String]()
  private val tacId2TacTitle = new HashMap[String,  String]()
  for(line <- f.getLines()){
    val sp = line.split("\t")
    val wikiId = Integer.parseInt(sp(0))

    val wikiTitle = sp(1).replaceAll(" ","_")
    val tacTitle = sp(2)

    // load wiki title. put underscores, build mapping wikititle -> tac id and back
    val tacId = sp(3)
    val tacType = sp(4)
    wikiId2TacId += (wikiId -> tacId)
    tacId2wikiId += (tacId -> wikiId)
    tacId2tacType += (tacId -> tacType)

    wikiTitle2TacId += (wikiTitle -> tacId)
    wikiTitle2WikiId += (wikiTitle -> wikiId)
    tacId2WikiTitle += (tacId -> wikiTitle)
    tacId2TacTitle += (tacId -> tacTitle)
  }
  f.close()

  val wikiId2tacIdMap = wikiId2TacId.result()
  val tacId2wikiIdMap = tacId2wikiId.result()
  val tacId2tacTypeMap = tacId2tacType.result()
  val tacId2WikiTitleMap = tacId2WikiTitle.result()
  val tacId2TacTitleMap = tacId2TacTitle.result()
  val wikiTitle2TacIdMap = wikiTitle2TacId.result()
  val wikiTitle2WikiIdMap = wikiTitle2WikiId.result().withDefault(wikititle => {-1})
}
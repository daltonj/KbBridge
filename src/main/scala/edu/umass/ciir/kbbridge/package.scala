package edu.umass.ciir

/**
 * User: jdalton
 * Date: 6/6/13
 */
package object kbbridge {

  type QueryId = String
  type EntityId = String
  type QueryJudgmentSet = Map[QueryId, Seq[Judgment]]
}

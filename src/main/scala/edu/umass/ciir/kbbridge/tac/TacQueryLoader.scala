package edu.umass.ciir.kbbridge.tac

import collection.mutable.ListBuffer
import collection.mutable
import edu.umass.ciir.kbbridge.data.TacEntityMention

/**
 * User: jdalton
 * Date: 4/1/13
 */
object TacQueryLoader {

  def loadQueries(annoFile: String, queryFile: String): Seq[TacEntityMention] = {
    //println("TacQueryLoader: load Query "+annoFile+"  "+queryFile)
    val queries = ListBuffer[TacEntityMention]()

    val queryIdToEnttType = new mutable.HashMap[String, String]()
    val queryIdToNodeId = new mutable.HashMap[String, String]()

    for (line <- io.Source.fromFile(annoFile).getLines()) {
      val chunks = line.split("\t")
      queryIdToEnttType += (chunks(0) -> chunks(2))
      queryIdToNodeId += (chunks(0) -> chunks(1))
    }




    val queryReader: TacQueryReader = new TacQueryReader(new TacQueryProcessor {
      def processQuery(q: TacQuery): Unit = {
        val enttype = queryIdToEnttType.getOrElse(q.getQueryId, "")

        val nodeId =
          if (q.getQueryEntity != null) {
            q.getQueryEntity
          } else if (q.getQueryNodeId != null) {
            q.getQueryNodeId
          } else {
            queryIdToNodeId.getOrElse(q.getQueryId, "")
          }
        // use entity for nodeid
        val fixedQ = TacEntityMention(docId = q.getQueryDocId
          , entityType = enttype
          , mentionId = q.getQueryId
          , entityName = q.getQueryName
          , nodeId = nodeId
        )


        queries += fixedQ
      }
    }, queryFile)

    queryReader.readQueryFile()

    queries
  }

}

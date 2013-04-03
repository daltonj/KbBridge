package edu.umass.ciir.kbbridge.features

import edu.umass.ciir.kbbridge.serial.EntityMentionProtos.TacEntityMentionLinkerFeatures
import edu.umass.ciir.kbbridge.data.EntityMention
import edu.umass.ciir.kbbridge.util.ConfInfo
import java.io.{FileInputStream, File}
import collection.mutable.ListBuffer

/**
 * User: jdalton
 * Date: 4/3/13
 */
object FileFeatureLoader {

  def loadProtobufDataForQueries(queries : Seq[EntityMention]) : Seq[TacEntityMentionLinkerFeatures] = {

    val instances = new ListBuffer[TacEntityMentionLinkerFeatures]

    for (query <- queries) {
    val filename = ConfInfo.serialComentionPath + File.separator + query.docId + "_" + query.mentionId + "_m2eOnly.pbdat"
      val featureFile = new File(filename)
      if (featureFile.exists()) {
        val instance = TacEntityMentionLinkerFeatures.parseFrom(new FileInputStream(featureFile))
        instances += instance
      } else {
        println("No feature file for query: " + query + " " + filename)
        None
      }
    }
    instances
  }

}

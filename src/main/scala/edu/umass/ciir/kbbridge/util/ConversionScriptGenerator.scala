package edu.umass.ciir.kbbridge.util

/**
 * User: jdalton
 * Date: 4/5/13
 */

import java.io.File
import java.io.PrintWriter
import edu.umass.ciir.kbbridge.tac.TacQueryUtil

object ConversionScriptGenerator {

  def main(args: Array[String]) {

    val outputFile = new File("./scripts/tac/link-tac-jobs-blake")

    val targetQueries = TacQueryUtil.allQueries

    val n = 50000
    var curBatch = 0
    var p = new PrintWriter(outputFile.getAbsolutePath() + curBatch.toString + ".sh", "UTF-8")
    for (querySet <- targetQueries grouped 100) {
      val sb = new StringBuilder
      sb append "qsub -b y " + /*-l mem_free=4G -l mem_token=4G*/ " -cwd -o ./out/"
      sb append querySet.head.mentionId
      sb append " -e ./err/"
      sb append querySet.head.mentionId

      sb append " /home/jdalton/kbbridge/scripts/tac/runEntityLinker.sh "
      //  sb append " /work1/allan/jdalton/tacco/scripts/runEntityLinker.sh "

      // input query
      sb append querySet.map(q => q.mentionId).mkString(",")

      // println(sb.toString)
      p.println(sb.toString)
    }
    p.close()
  }

}

package edu.umass.ciir.kbbridge.features.util

import java.io.PrintWriter
import collection.mutable.HashMap
import edu.umass.ciir.kbbridge.util.ConfInfo

object TermCountsMap {

  private val termFrequencyCounts = loadMap

  def loadMap() : HashMap[String,  (Long, Long)] = {
    val termFrequencyMap = new HashMap[String,  (Long, Long)]()
     try {
  val f = io.Source.fromFile(ConfInfo.galagoTermCounts)
  for (line <- f.getLines()) {
    val chunks = line.split("\t")
    val term = chunks(0)
    val termFrequency = chunks(1).toLong
    val documentFrequency = chunks(2).toLong
    if (termFrequency > 1) {
      termFrequencyMap += term -> (termFrequency, documentFrequency)
    }
  }
  f.close
  } catch {
    case e:Exception => println("Error loading term counts! + " + e)
    throw e
  }
  println("Term count size : " + termFrequencyMap.size)
  termFrequencyMap
  }
  
  val termFrequencyCountsMap = termFrequencyCounts.result().withDefault(term => (1,1))
  
  def main(args: Array[String]) {
      val p = new PrintWriter(args(0) , "UTF-8")
      val map = TermCountsMap.termFrequencyCountsMap
      for (k <- map.keys) {
        val result = map(k)
        val term = k
        val termFrequency = result._1
        val documentFrequency = result._2
        if (k.split("\\s+").size == 1 && termFrequency > 1) {
            p.println(k + "\t" + termFrequency + "\t" + documentFrequency)
        }
      }
      p.close
    }
}
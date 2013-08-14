package edu.umass.ciir.kbbridge

import org.lemurproject.galago.core.retrieval.ScoredDocument;
import scala.collection.mutable.ListBuffer
import java.io._
import scala.io.Source
import scala.collection.immutable.Map
import scala.collection.JavaConversions._

case class QueryDocument(val query:QueryId, val name:String, val m_rank:Int, var m_score:Double, val contextSource:String) extends ScoredDocument(name, m_rank, m_score)

object RunFileLoader {

   def readRunFileWithQuery(runFile: File, resultLimit: Int = 1000) : Map[QueryId, Seq[ScoredDocument]] = {
  
//    println("Loading run file: " + runFile.getAbsolutePath())
    val runResults = new ListBuffer[QueryDocument]()
    val source = Source.fromFile(runFile, "UTF-8")
    for (line <- source.getLines ) {
      val data = line.split("\\s+")
      if (data.length != 6) {
        throw new Exception("Wrong number of fields in line!" + line)
      }
      val doc = QueryDocument("%03d".format(data(0).toInt) , data(2), data(3).toInt, data(4).toDouble, runFile.getName())
      doc.source = doc.contextSource
      runResults += doc
    }
    val resultsByQuery = runResults.groupBy(t => t.query).filterNot(queries => queries._1 equals "672")
    val resultsbyQuerySorted = resultsByQuery.map( f => {
      val sorted = f._2.sortBy(d => d.rank) take resultLimit
      
      (f._1, sorted.toSeq)
    })
  //  resultsbyQuerySorted.map(f => Util.maxMinNorm(f._2))
    resultsbyQuerySorted
  }
   
}
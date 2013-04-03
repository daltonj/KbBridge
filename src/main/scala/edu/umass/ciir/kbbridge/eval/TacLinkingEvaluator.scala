package edu.umass.ciir.kbbridge.eval

import collection.mutable.{HashMap, ListBuffer}
import java.io.PrintWriter

object TacLinkingEvaluator {
  
   val allQueries = new HashMap[String,  String]()
   val nilQueries = new HashMap[String,  String]()
   val nonNilQueries = new HashMap[String,  String]()
   
   case class EvalResults ( accOnAll:Double, accOnNonNil:Double, accOnNil:Double, nilAcc:Double ) {
    override def toString:String = {
      "=================\n" +
        "Evaluation Results\n" +
        "Accuracy\n"+
        "all:\t"+accOnAll+"\n"  +
        "non-Nil:\t"+accOnNonNil+"\n" +
        "Nil:\t"+accOnNil+"\n" +
        "Nil-accuracy:\t"+nilAcc
    }
  }
   
   def loadGoldStandardFile(goldStandardFile : String) {
     
     for(line <- io.Source.fromFile(goldStandardFile).getLines()) {
        val chunks = line.split("\\s+")
        val mention = chunks(0)
        val kbId = chunks(1)
        
        allQueries += (chunks(0) -> chunks(1))
        if (kbId startsWith "NIL") {
          nilQueries += (chunks(0) -> chunks(1))
        } else {
          nonNilQueries += (chunks(0) -> chunks(1))
        }
      }
     println("Loaded gold standard file: " + goldStandardFile + " with: " + allQueries.keySet.size)
   }
   
   def clearJudgments() {
     allQueries.clear()
     nilQueries.clear()
     nonNilQueries.clear()

   }
   
   def microAverageAccuracy(queries: Seq[String], predictions: HashMap[String, (Seq[String], Double)], writer:PrintWriter, description:String, printResults:Boolean=false) : Double =  {
     
     val numSamples = queries.size.toDouble
     var numCorrect = 0;
     
     
     for (query <- queries) {
       val kbId = allQueries(query)
       val (answers, score) = predictions.getOrElse(query, (List("NONE"),-1))
       val kbNormalizedName = if (kbId.startsWith("NIL")) { "NIL" } else { kbId }
       
       if (answers.length > 1 ) {
         // use set overlap
         
        val pageSet = answers.toSet
        if (pageSet contains kbId) {
          numCorrect +=1
        } else {
          // eehh wrong.
        }
         
       } else {
       val answerId = answers(0)
       val queryName = if (answerId.startsWith("NIL")) {"NIL"} else {answerId}
       if (queryName equals kbNormalizedName) {
         if (printResults) {
           writer.println(query + "\t" + kbId + " \t" +  answerId + "\t" + "%10.4f".format(score) + "\t" + true)
         }
         numCorrect += 1
       } else {
         // WRONG!
         if (printResults) {
           writer.println(query + "\t" + kbId + " \t" +  answerId + "\t" + "%10.4f".format(score) + "\t" + false)
         }
       }
       }
     }
     val accuracy = numCorrect.toDouble / numSamples
     writer.println(description + "\t" + numCorrect + "\t" + numSamples + "\t" + accuracy)
     
     //println("correct:" + numCorrect + " total: " + numSamples + " Acc:" + accuracy)
     accuracy
   }
   
   def nilAccuracy(queries: Seq[String], predictions: HashMap[String, (Seq[String], Double)], writer:PrintWriter, description:String, printResults:Boolean=false) : Double =  {
     
     val numSamples = queries.size.toDouble
     var numCorrect = 0;
     
     for (query <- queries) {
       val kbId = allQueries(query)
       val (answers, score) = predictions.getOrElse(query, (List("NIL"),-1))
       val kbNormalizedName = if (kbId.startsWith("NIL")) { "NIL" } else { "Some" }
       
        val answerId = answers(0)
       val queryName = if (answerId.startsWith("NIL")) {"NIL"} else {"Some"}
       if (queryName equals kbNormalizedName) {
         if (printResults) {
           writer.println(query + "\t" + kbId + " \t" +  answerId + "\t" + "%10.4f".format(score) + "\t" + true)
         }
         numCorrect += 1
       } else {
         // WRONG!
         if (printResults) {
           writer.println(query + "\t" + kbId + " \t" +  answerId + "\t" + "%10.4f".format(score) + "\t" + false)
         }
       }       
     }
     val accuracy = numCorrect.toDouble / numSamples
     writer.println(description + "\t" + numCorrect + "\t" + numSamples + "\t" + accuracy)
     
     //println("correct:" + numCorrect + " total: " + numSamples + " Acc:" + accuracy)
     accuracy
   }
   
  
}
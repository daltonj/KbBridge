package edu.umass.ciir.kbbridge.eval

object GalagoEvaluator {

  import org.lemurproject.galago.core.eval.QuerySetJudgments;
  import org.lemurproject.galago.core.eval.QuerySetResults;

  import org.lemurproject.galago.core.eval.aggregate.QuerySetEvaluatorFactory;
  import org.lemurproject.galago.tupleflow.Parameters;
  import org.lemurproject.galago.core.retrieval.ScoredDocument;
  import scala.collection.JavaConversions._
  import collection.mutable.HashMap
  import collection.mutable.ListBuffer
  import java.io.PrintWriter

  val metrics = Array("map", "P1");
  val evalFormat = "%2$-16s%1$3s %3$6.5f";

  def evaluate(annotationFile: String, resultMap: HashMap[String, Array[ScoredDocument]], outputFilePrefix: String): (Seq[(String, Double)], Map[String, ListBuffer[Double]]) = {
    val writer = new PrintWriter(outputFilePrefix + ".galago_eval")
    val qsr = new QuerySetResults(resultMap);
    val qrelFile = annotationFile.replace(".tab", ".qrel")
    val qrels = new QuerySetJudgments(qrelFile,true,true)
    val evaluators = for (metric <- metrics) yield {
      QuerySetEvaluatorFactory.instance(metric, new Parameters())
    }

    val queryByQueryResults = new HashMap[String, ListBuffer[Double]]
    val summaryResults = new ListBuffer[(String, Double)]

    for (query <- qsr.getQueryIterator()) {
      for (evaluator <- evaluators) {
        val evalScore = evaluator.evaluate(qsr.get(query), qrels.get(query))
        writer.println(evalFormat.format(query, evaluator.getMetric(), evalScore))
        queryByQueryResults.getOrElseUpdate(evaluator.getMetric(), ListBuffer()) += evalScore
      }
    }

    evaluators.map(evaluator => {
      val summaryScore = evaluator.evaluate(qsr, qrels)
      writer.println(evalFormat.format("all", evaluator.getMetric(), summaryScore))
      summaryResults += ((evaluator.getMetric(), summaryScore))
    })
    writer.close
    (summaryResults.toSeq, queryByQueryResults.toMap)
  }


}
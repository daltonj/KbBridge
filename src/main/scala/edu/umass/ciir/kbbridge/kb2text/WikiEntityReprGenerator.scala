//package edu.umass.ciir.kbbridge.kb2text
//
//import scala.collection.JavaConversions._
//import edu.umass.ciir.kbbridge.util.ConfInfo
//import edu.umass.ciir.kbbridge.data.{WikipediaEntity, EntityMention}
//import edu.umass.ciir.kbbridge.nlp.{NlpQueryContextBuilder, PseudoRelevanceNerReweighter, NaiveQueryContextBuilder, TextNormalizer}
//import edu.umass.ciir.models.LanguageModel
//import edu.umass.ciir.kbbridge.data.repr.EntityRepr
//
//
//class WikiEntityReprGenerator() {
//
//  def createQEntityRepr(query: WikipediaEntity): EntityRepr = {
//    val entityName = query.wikipediaTitle.replaceAllLiterally("_", " ")
//    EntityRepr(entityName = entityName)
//  }
//
//  def createQVEntityRepr(query: WikipediaEntity): EntityRepr = {
//    val entityName = query.wikipediaTitle.replaceAllLiterally("_", " ")
//    val nameVariants = createNameVariantsContext(query)
//    EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId), nameVariants = nameVariants)
//  }
//
////  def createQVMEntityRepr(query: WikipediaEntity): EntityRepr = {
////    val nameVariants = createNameVariantsContext(query)
////    val neighborHood = convertNeighborNameToEntityRepr(createNeighborNameContext(query, nameVariants.map(_._1)))
////    EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId), nameVariants = nameVariants, neighbors = neighborHood)
////  }
////
////  def createQVMSEntityRepr(query: WikipediaEntity): EntityRepr = {
////    val nameVariants = createNameVariantsContext(query)
////    val neighborHood = convertNeighborNameToEntityRepr(createNeighborNameContext(query, nameVariants.map(_._1)))
////    val wordContext = createWordContext(query)
////    EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId), nameVariants = nameVariants, neighbors = neighborHood, words = wordContext)
////  }
////
//
//
////
////
////  def createWordContext(query:EntityMention):Seq[(String, Double)] = {
////    val normtext = TextNormalizer.normalizeText(query.fullText)
////    val normTokens = normtext.split("\\s+")
////    val unigramModel = new LanguageModel(1)
////    unigramModel.addDocument(normTokens, true)
////    unigramModel.calculateProbabilities()
////
////    val unigramWeights = unigramModel.getKTopFrequencies(20, true, 1)
////    unigramWeights.toSeq.map(uw => uw.getTerm -> uw.getProbability)
////  }
//
//  def createNameVariantsContext(query:WikipediaEntity):Seq[(String, Double)] = {
//    val text = query.metadata.get("xml")
//
//    Seq()
//  }
//
//
//  def convertNeighborNameToEntityRepr(neighborname:Seq[(String,Double)]):Seq[(EntityRepr, Double)] = {
//    for((name, weight) <- neighborname) yield {
//      new EntityRepr(entityName = name, queryId = None) -> weight
//    }
//  }
//
//  def createNeighborNameContext(query:EntityMention, nameVariants:Seq[String]):Seq[(String, Double)]= {
//
//    val nerContextBuilder = new NlpQueryContextBuilder
//    val nerContext = nerContextBuilder.buildContext(query)
//    val nerReweighter = new PseudoRelevanceNerReweighter()
//
//
//
//    val setOfContextNers = ConfInfo.nerNeighborQuerySelectMethod match {
//      case "all" => {
//        println("all: Using all ners from the query document")
//        nerContext.allNersSorted
//      }
//      case "kclosest" => {
//        println("kclosest: Using k closest ners")
//        nerContext.allNersSorted.take(ConfInfo.nerNeighborQueryK)
//      }
//
//    }
//
//
//
//    val nerWeights =
//      ConfInfo.nerNeighborQueryMethod match {
//        case "uniform" => {
//          nerReweighter.buildTrivialReweights(query, setOfContextNers, nameVariants)
//        }
//        case "local" => {
//          nerReweighter.buildLocalWeights(query, setOfContextNers, nameVariants)
//        }
//        case "discount" => {
//          nerReweighter.reweightNersWithPseudorel(query, setOfContextNers,nameVariants, Seq())
//        }
//        case "discountadd" => {
//          nerReweighter.reweightAddNersWithPseudorel(query, setOfContextNers, nameVariants , Seq(), addPseudoNers = true)
//        }
//        case _ => throw new Error("Option for ConfInfo.nerNeighborQueryMethod  not supported")
//      }
//
//
//    nerWeights.toSeq
//  }
//
//
//
//
//
//  /**
//   * For backwards compatibility
//   */
//  def createEntityReprForBackwardsCompatibility(query: EntityMention): EntityRepr = {
//
//    if (ConfInfo.useNerContextInQuery) {
//      entityReprFromCorefAndNerContext(query)
//    } else {
//      entityReprFromCorefContext(query)
//    }
//
//  }
//
//
//  /**
//   * For backwards compatibility
//   */
//  def entityReprFromCorefAndNerContext(query: EntityMention): EntityRepr = {
//
//    val corefContextBuilder = new NaiveQueryContextBuilder
//    val corefContext = corefContextBuilder.buildContext(query)
//
//
//    val nerContextBuilder = new NlpQueryContextBuilder
//    val nerContext = nerContextBuilder.buildContext(query)
//    val nerReweighter = new PseudoRelevanceNerReweighter()
//
//
//    val normSentences = nerReweighter.getNormSentences(query)
//
//    val setOfContextNers = ConfInfo.nerNeighborQuerySelectMethod match {
//      case "all" => {
//        println("all: Using all ners from the query document")
//        nerContext.allNersSorted
//      }
//      case "sentence" => {
//        println("sentence: Using ners mentioned in the sentences mentioning the query")
//        nerReweighter.sentenceFilterNers(query, nerContext.allNersSorted, nerContext.altNames, normSentences)
//        //        nerContext.contextNers
//        //nerReweighter.buildSentenceFilterReweights(tacQuery, nerContext.contextNers,corefContext.altNames)
//      }
//      case "kclosest" => {
//        println("kclosest: Using k closest ners")
//        nerContext.allNersSorted.take(ConfInfo.nerNeighborQueryK)
//      }
//
//    }
//
//
//    val normQuery = TextNormalizer.normalizeText(query.entityName)
//    val selectedSentences = normSentences.filter(sent => sent.contains(normQuery)).take(5).map(sent => {
//      if (sent.length > 200)
//        sent.substring(0, math.min(sent.length, math.max(200, sent.indexOf(" ", 200))))
//      else sent
//    })
//
//    println("enriching query with sentences:\n" + selectedSentences.mkString("\n"))
//
//
//    val normtext = TextNormalizer.normalizeText(query.fullText)
//    val normTokens = normtext.split("\\s+")
//    val unigramModel = new LanguageModel(1)
//    unigramModel.addDocument(normTokens, true)
//    unigramModel.calculateProbabilities()
//
//    val unigramWeights = unigramModel.getKTopFrequencies(20, true, 1)
//
//
//    val nerWeights =
//      ConfInfo.nerNeighborQueryMethod match {
//        case "uniform" => {
//          nerReweighter.buildTrivialReweights(query, setOfContextNers, corefContext.altNames)
//        }
//        case "local" => {
//          nerReweighter.buildLocalWeights(query, setOfContextNers, corefContext.altNames)
//        }
//        case "discount" => {
//          nerReweighter.reweightNersWithPseudorel(query, setOfContextNers, corefContext.altNames, selectedSentences)
//        }
//        case "discountadd" => {
//          nerReweighter.reweightAddNersWithPseudorel(query, setOfContextNers, corefContext.altNames, selectedSentences, addPseudoNers = true)
//        }
//        case _ => throw new Error("Option for ConfInfo.nerNeighborQueryMethod  not supported")
//      }
//
////    val uniformNer = nerReweighter.buildTrivialReweights(query, setOfContextNers, corefContext.altNames)
////    val localNer = nerReweighter.buildLocalWeights(query, setOfContextNers, corefContext.altNames)
////    // val rmReweightedNer = nerReweighter.reweightNersWithPseudorel(query, setOfContextNers, corefContext.altNames, selectedSentences)
////    // val rmNer = nerReweighter.reweightAddNersWithPseudorel(query, setOfContextNers, corefContext.altNames, selectedSentences, addPseudoNers = true)
////
////    val nerWeightNoStop = nerWeights.filter(entry => {
////      val string = entry._1
////      stringShortOrOnlyStopwords(string)
////    })
////
////    val useSelSentences =
////      if (ConfInfo.useSentencesInCandidateQuery) {
////        selectedSentences
////      } else Seq()
////
//    println("running first pass with context... (v 0.1)")
//
//
//    val nameVariants =uniformWeighting(corefContext.altNames.toList)
//    val resultRepr = EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId), nameVariants = nameVariants)
//
//
//
//
//    //    galagoSearcher.searchComponents(new Query(query.mentionId, query.entityName, numCands),
//    //    corefContext.altNames.toList, toJavaNeighborWeights(nerWeights), firstPassResults, useSelSentences,
//    //    toJavaNeighborWeights(uniformNer), toJavaNeighborWeights(localNer), toJavaNeighborWeights(rmReweightedNer),
//    //    toJavaNeighborWeights(rmNer)).toList
//
//    resultRepr
//
//  }
//
//
//
//  /**
//   * For backwards compatibility
//   */
//  def entityReprFromCorefContext(query: EntityMention): EntityRepr = {
//
//    var contextBuilder = new NaiveQueryContextBuilder
//    var context = contextBuilder.buildContext(query)
//
//
//    val nameVariants = uniformWeighting(context.altNames.toList)
//    EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId), nameVariants = nameVariants)
//
//  }
//
//
//  def uniformWeighting[A](seq:Seq[A]):Seq[(A,Double)] = {
//    val weight = 1.0/seq.length
//    seq.map( (_ -> weight))
//  }
//
////  def toJavaNeighborWeights(map: Map[String, Double]): util.Map[String, lang.Double] = {
////    map.filter(!_._2.isNaN).map(entry => (entry._1, new java.lang.Double(entry._2))).toMap[String, lang.Double]
////  }
////
////
////  def stringShortOrOnlyStopwords(string: String): Boolean = {
////    string.trim().length > 2 &&
////      !string.split("\\s+").forall(token => StopWordList.isStopWord(token.toLowerCase))
////  }
//
//
//
//
//  def close() {
//  }
//
//}

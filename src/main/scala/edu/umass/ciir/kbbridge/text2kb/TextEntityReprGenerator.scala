package edu.umass.ciir.kbbridge.text2kb

import edu.umass.ciir.kbbridge.data.EntityMention
import edu.umass.ciir.kbbridge.data.repr.EntityRepr
import edu.umass.ciir.kbbridge.nlp.{TextNormalizer, PseudoRelevanceNerReweighter, NlpQueryContextBuilder, NaiveQueryContextBuilder}
import edu.umass.ciir.kbbridge.util.ConfInfo
import edu.umass.ciir.models.LanguageModel
import scala.collection.JavaConversions._

/**
 * User: dietz
 * Date: 6/10/13
 * Time: 2:12 PM
 */
trait TextEntityReprGenerator {
  def createEntityRepr(query:EntityMention):EntityRepr
}

object TextEntityReprGeneratorsUtil {

  def createNameVariantsContext(query:EntityMention):Seq[(String, Double)] = {
    val corefContextBuilder = new NaiveQueryContextBuilder
    val corefContext = corefContextBuilder.buildContext(query)

    val nameVariants = uniformWeighting(corefContext.altNames)
    nameVariants
  }


  def createWordContext(query:EntityMention):Seq[(String, Double)] = {
    val text = query.fullText
    createWeightedWords(text)
  }


  def createWeightedWords(text: String): Seq[(String, Double)] = {
    val normtext = TextNormalizer.normalizeText(text)
    val normTokens = normtext.split("\\s+")
    val unigramModel = new LanguageModel(1)
    unigramModel.addDocument(normTokens, true)
    unigramModel.calculateProbabilities()

    val unigramWeights = unigramModel.getKTopFrequencies(20, true, 1)
    unigramWeights.toSeq.map(uw => uw.getTerm -> uw.getProbability)
  }

  def convertNeighborNameToEntityRepr(neighborname:Seq[(String,Double)]):Seq[(EntityRepr, Double)] = {
    for((name, weight) <- neighborname) yield {
      new EntityRepr(entityName = name, queryId = None) -> weight
    }
  }

  def createNeighborNameContext(query:EntityMention, nameVariants:Seq[String]):Seq[(String, Double)]= {

    val nerContextBuilder = new NlpQueryContextBuilder
    val nerContext = nerContextBuilder.buildContext(query)
    val nerReweighter = new PseudoRelevanceNerReweighter()



    val setOfContextNers = ConfInfo.nerNeighborQuerySelectMethod match {
      case "all" => {
        println("all: Using all ners from the query document")
        nerContext.allNersSorted
      }
      case "kclosest" => {
        println("kclosest: Using k closest ners")
        nerContext.allNersSorted.take(ConfInfo.nerNeighborQueryK)
      }

    }

    val nerWeights =  nerReweighter.buildLocalWeights(query, setOfContextNers, nameVariants)

//
//
//    val nerWeights =
//      ConfInfo.nerNeighborQueryMethod match {
//        case "uniform" => {
//          nerReweighter.buildTrivialReweights(query, setOfContextNers, nameVariants)
//        }
//        case "local" => {
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

    nerWeights.toSeq
  }

  def uniformWeighting[A](seq:Seq[A]):Seq[(A,Double)] = {
    val weight = 1.0/seq.length
    seq.map( (_ -> weight))
  }

}


object QTextEntityRepr extends TextEntityReprGenerator {
  def createEntityRepr(query: EntityMention) = {
    EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId))
  }
}
object QVTextEntityRepr extends TextEntityReprGenerator {
  import TextEntityReprGeneratorsUtil._
  def createEntityRepr(query: EntityMention) = {
    val nameVariants = createNameVariantsContext(query)
    EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId), nameVariants = nameVariants)
  }
}
object QVSTextEntityRepr extends TextEntityReprGenerator {
  import TextEntityReprGeneratorsUtil._
  def createEntityRepr(query: EntityMention) = {
    val nameVariants = createNameVariantsContext(query)
    val wordContext = createWordContext(query)
    EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId), nameVariants = nameVariants, words = wordContext)
  }
}
object QVSMLocalTextEntityRepr extends TextEntityReprGenerator {
  import TextEntityReprGeneratorsUtil._
  def createEntityRepr(query: EntityMention) = {
    val nameVariants = createNameVariantsContext(query)
    val neighborHood = convertNeighborNameToEntityRepr(createNeighborNameContext(query, nameVariants.map(_._1)))
    val wordContext = createWordContext(query)
    EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId), nameVariants = nameVariants, neighbors = neighborHood, words = wordContext)
  }
}
object QVMLocalTextEntityRepr extends TextEntityReprGenerator {
  import TextEntityReprGeneratorsUtil._
  def createEntityRepr(query: EntityMention) = {
    val nameVariants = createNameVariantsContext(query)
    val neighborHood = convertNeighborNameToEntityRepr(createNeighborNameContext(query, nameVariants.map(_._1)))
    EntityRepr(entityName = query.entityName, queryId = Some(query.mentionId), nameVariants = nameVariants, neighbors = neighborHood)
  }
}

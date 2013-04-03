package edu.umass.ciir.kbbridge.features

import edu.umass.ciir.kbbridge.data.{ScoredWikipediaEntity, WikipediaEntity, EntityMention}


/**
 *  We do not add topic features
 */
object GalagoEntityLinkingFeatureLib {

  object GalagoEntityLinkingFeatures{
    val featurePrefix = "GalagoEntityLinkingFeatures"

    val galagoScore1 = "galagoscore1"
    val galagoScoreNorm = "galagoscoreNorm"
      
    
  }
  trait GalagoEntityLinkingFeatures extends FeatureGenerator {
    import GalagoEntityLinkingFeatures._


     def generateGalagoEntityLinkingFeatures(mention:EntityMention, entity:ScoredWikipediaEntity, otherCands:Seq[ScoredWikipediaEntity]) {
      
      addUnprefixedValueFeature("galagoscoreraw", entity.score)
      var prob =  Math.exp(entity.score)
      addUnprefixedValueFeature("galagoscoreexp", prob)

      otherCands.headOption match {
        case Some(h) => {
          val K = h.score
          var sum = 0.0d
          //sum += prob

          for (cand <- otherCands) {
            sum += Math.exp(cand.score - K)
          }

          val normalized = Math.exp(h.score - (K + math.log(sum)))
          addUnprefixedValueFeature(galagoScoreNorm, normalized)
        }
        case None => {
        }
      }
    }

    private def addUnprefixedFeature(category:String, value:String) {
      addFeature(featurePrefix, category, value)
    }
    private def addUnprefixedValueFeature(category:String, value:Double) {
      addValueFeature(featurePrefix, category, value)
    }

  }


}
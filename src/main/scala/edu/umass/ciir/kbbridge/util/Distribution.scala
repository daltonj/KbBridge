package edu.umass.ciir.kbbridge.util

import edu.umass.ciir.kbbridge.prf.LogTools

/**
 * User: dietz
 * Date: 8/30/13
 * Time: 12:09 PM
 */
case class Distribution[Elem](nonzeroDistr: Seq[(Elem, Double)], zeroEntries: Int = 0,
                              zeroEntryNames: PartialFunction[Int, Elem] = Seq.empty, zeroCountProb: Double = 0.0,
                              isLog: Boolean = false
                             ) extends CategoricalCompressedDistribution[Elem] {
  type concreteType = Distribution[Elem]

  assert(nonzeroDistr.map(_._1).distinct.length == nonzeroDistr.length, {
    "input for nonzeroDistr is expected to only contain unique elements. But distinct elements=" + nonzeroDistr.map(_._1).distinct.length + " num elements=" + nonzeroDistr.length + ". NonZeroDistr = " + nonzeroDistr.toList
  })
  if (zeroEntries > 0) assert(zeroEntryNames.isDefinedAt(zeroEntries - 1), {
    "zeroEntryNames are not defined at position " + (zeroEntries - 1) + " zeroEntryNames=" + (0 until zeroEntries).map(zeroEntryNames).toSeq
  })

  def nonzeroHashView: Map[Elem, Double] = nonzeroDistr.toMap

  def builtNonZeroDistrFrom(distr: Seq[(Elem, Double)], zeroEntries: Int, zeroEntryNames: PartialFunction[Int, Elem],
                            zeroCountProb: Double, isLog: Boolean
                           ) = {
    Distribution(distr, zeroEntries, zeroEntryNames, zeroCountProb, isLog)
  }

  def builtDistrFrom(distr: Seq[(Elem, Double)], isLog: Boolean) = {
    Distribution(nonzeroDistr = distr, 0, zeroEntryNames, zeroCountProb, isLog)
  }

  override def toString = {
    "Distribution(" + nonzeroDistr.sortBy(-_._2).toString + " " + (0 until zeroEntries).map(zeroEntryNames.applyOrElse(_,
                                                                                                                       "??") -> zeroCountProb).toString + ")"
  }
}

/**
 * User: dietz
 * Date: 8/14/13
 * Time: 5:16 PM
 */
trait CategoricalCompressedDistribution[Elem] {
  assert(!(nonzeroDistr.isEmpty && zeroEntries == 0), {
    "Can't instantiate empty distribution"
  })
  type concreteType <: CategoricalCompressedDistribution[Elem]

  def builtDistrFrom(distr: Seq[(Elem, Double)], isLog: Boolean): concreteType

  def builtNonZeroDistrFrom(distr: Seq[(Elem, Double)], zeroEntries: Int, zeroEntryNames: PartialFunction[Int, Elem],
                            zeroCountProb: Double, isLog: Boolean
                           ): concreteType

  def nonzeroDistr: Seq[(Elem, Double)]

  def nonzeroHashView: Map[Elem, Double]

  def zeroEntries: Int

  def zeroEntryNames: PartialFunction[Int, Elem]

  def zeroCountProb: Double

  def isLog: Boolean

  def distr = {
    nonzeroDistr ++ (0 until zeroEntries).map(zeroEntryNames(_) -> zeroCountProb)
  }

  def universe: Iterable[Elem] = {
    nonzeroDistr.map(_._1) ++ (0 until zeroEntries).map(zeroEntryNames)
  }

  def topK(k: Int): concreteType = {
    val nonzeroPart = SeqTools.topK(nonzeroDistr, k)
    val numZeros = math.min(k - nonzeroPart.length, zeroEntries)
    builtNonZeroDistrFrom(nonzeroPart, numZeros, zeroEntryNames, zeroCountProb, isLog)
  }

  def normalize: concreteType = {
    if (isLog) logExpSumNormalize
    else divideByMarginal
  }

  def divideByMarginal: concreteType = {
    val marg = marginal
    builtNonZeroDistrFrom(nonzeroDistr.map(elem => (elem._1, elem._2 / marg)), zeroEntries, zeroEntryNames,
                          zeroCountProb / marg, isLog)
  }

  def logExpSumNormalize: concreteType = {
    builtDistrFrom(LogTools.logExpSumNormalizeBase(distr), isLog = false)
  }

  def getArgMax: (Elem, Double) = {
    val nonZeroEntry = nonzeroDistr.maxBy(_._2)
    if (nonZeroEntry._2 < zeroCountProb && zeroEntries > 0) {
      (zeroEntryNames(0) -> zeroCountProb)
    } else {
      nonZeroEntry
    }
  }

  def marginal: Double = {
    assert(!isLog)
    assert(nonzeroDistr.forall(_._2 >= 0) || zeroCountProb > 0.0 && zeroEntries > 0)
    nonzeroDistr.view.map(_._2).sum + zeroEntries * zeroCountProb
  }


  /**
   *
   * @param random a random number between 0 and marginal
   * @return
   */
  def draw(random: Double): Elem = {
    if (nonzeroDistr.isEmpty && zeroEntries == 0) throw new RuntimeException("can't draw from a CategoricalDistribution with no elements")
    if (random < 0.0) throw new RuntimeException("random seed must be >= 0.0")
    if (random > marginal) throw new RuntimeException("random seed must be <= this.marginal")

    def op(thread: (Option[Elem], Double), entry: (Elem, Double)
          ): (Option[Elem], Double) = (Some(entry._1), thread._2 + entry._2)
    val virtualFullDistr = nonzeroDistr.view ++ (0 until zeroEntries).view.map(zeroEntryNames(_) -> zeroCountProb)
    val scanned: Iterable[(Option[Elem], Double)] = virtualFullDistr.scanLeft[(Option[Elem], Double), Iterable[(Option[Elem], Double)]]((None, 0.0))(op)

    val elem = scanned.dropWhile(_._2 <= random).headOption match {
      case Some((Some(elem), _)) => elem
      case None => {
        virtualFullDistr.last._1 // happens if random ==  marg, even for zeroEntries=0, then return the last element as well
      }
      case Some((None, _)) => {
        // this should not happen
        throw new RuntimeException("Program error: did not find any element in " + virtualFullDistr + " that represents cummulative probability " + random + ".")
      }
    }
    elem
  }

  def expectedCounts(expectedLength: Double): Map[Elem, Int] = {
    assert(!isLog)
    (for (elem <- universe) yield {
      val mass = prob(elem) * expectedLength
      elem -> Distribution.expectation2count(mass)
    }).toMap

  }


  def size: Int = {
    nonzeroHashView.size + zeroEntries
  }

  def prob(elem: Elem): Double = {
    val p =
      nonzeroHashView.get(elem) match {
        case Some(value) => value
        case None => zeroCountProb
      }
    if (isLog) math.exp(p)
    else p
  }


  def head: (Elem, Double) = {
    if (zeroEntries > 0) zeroEntryNames(0) -> zeroCountProb
    else {
      nonzeroDistr.head
    }
  }

  def logarithmize: CategoricalCompressedDistribution[Elem] = {
    builtNonZeroDistrFrom(distr.map(entry => (entry._1, math.log(entry._2))), zeroEntries, zeroEntryNames,
                          math.log(zeroCountProb), isLog = true)
  }
}

object Distribution {
  def singletonDistr[Elem](elem: Elem): Distribution[Elem] = {
    Distribution(Seq(elem -> 1.0), isLog = false)
  }

  def unionUniverse[Elem](weightedDistrs: Distribution[Distribution[Elem]]): IndexedSeq[Elem] = {
    weightedDistrs.universe.flatMap(_.universe).toSet.toIndexedSeq
  }

  def sumDuplicateKeys[Elem](nonZeroDistr: Seq[(Elem, Double)]): Seq[(Elem, Double)] = {
    SeqTools.sumSeq(nonZeroDistr)
  }

  def mix[Elem](elemUniverse: IndexedSeq[Elem], weightedDistrs: Distribution[Distribution[Elem]]
               ): Distribution[Elem] = {
    val normWeightedDistrs = weightedDistrs.normalize
    val mixedDistrProbs =
      for (elem <- elemUniverse) yield {
        val mixedProb =
          normWeightedDistrs.distr.map {
                                         case (distr, weight) => {
                                           val y = weight
                                           distr.prob(elem) * y
                                         }
                                       }.sum
        elem -> mixedProb
      }
    Distribution(mixedDistrProbs)
  }

  def createDistribution[Elem](nonZero: Seq[(Elem, Double)], universe: Seq[Elem], zeroCountProb: Double = 0.0,
                               isLog: Boolean = false
                              ): Distribution[Elem] = {
    val unseenKeys = universe diff nonZero.map(_._1)
    Distribution[Elem](nonZero, universe.size - nonZero.size, zeroEntryNames = unseenKeys,
                       zeroCountProb = zeroCountProb, isLog = isLog)
  }


  def expectation2count[Elem](mass: Double): Int = {
    val floormass = mass.floor.toInt
    val add = {
      if (math.random < (mass - floormass)) 1
      else 0
    }
    (floormass + add)
  }

  //  def mixExpectedCounts[Elem](expectedLength:Double, weightedDistrSeqs:Seq[(Seq[Distribution[Elem]],Double)]):Map[Elem,Int] = {
  //    weightedDistrSeqsIndexed = weightedDistrSeqs.toIndexedSeq
  //    val outerSeq: Seq[(Int, Nothing)] = for (((distr, weighted), idx) <- weightedDistrSeqs.zipWithIndex) yield idx -> weight
  //    val outerDistr = Distribution[Int](outerSeq, isLog = true).normalize
  //
  //
  //
  //  }
}
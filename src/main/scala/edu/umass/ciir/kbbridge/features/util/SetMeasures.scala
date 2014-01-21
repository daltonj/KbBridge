package edu.umass.ciir.kbbridge.features.util

case class SetMeasures[A](set1:Set[A], set2:Set[A]) {
  val intersect = (set1 intersect set2).size
  
  val missNum = set1.size - intersect + set2.size - intersect
  
  val approxPMI = {
   if (set1.size == 0 ||  set2.size==0) {
     0.0
   } else {
     1000.0 * intersect / (set1.size.toDouble * set2.size)
   }
  }

  def PMI(collectionSize: Long) : Double = {
    if (set1.size == 0 ||  set2.size==0) {
      0.0
    } else {
      (intersect / collectionSize.toDouble) / ((set1.size/ collectionSize.toDouble) * (set2.size/ collectionSize.toDouble))
    }
  }
  
 val size = set1.size + set2.size
 
  val diceCorrect = {
    if (size == 0) {
      0.0
    } else {
      (2.0 * intersect) / size.toDouble
    }
  }
  
 val union = (set1 ++ set2).size
 
 val jaccard = {
    if (union == 0) {
      0.0
    } else {
      1.0 * intersect / union.toDouble
    }
  }
 
 /**
  * collectionSize = num docs in collection
  */
 def normalizedGoogleDistance(collectionSize: Long) : Double = {
   if(set1.size==0 || set2.size==0|| intersect==0) {
     0.0
   } else {
     val distance = (Math.log(Math.max(set1.size, set2.size))-Math.log(intersect)) /
            (Math.log(collectionSize)- Math.log(Math.min(set1.size, set2.size)))
      1.0-distance
   }
        
 }
 
 def cosine = {
   if (set1.size==0 || set2.size==0|| intersect==0) {
     0.0
   } else {
   val weight1 = 1 / Math.sqrt(set1.size)
   val weight2 = 1 / Math.sqrt(set2.size)
   
   var cosine = intersect * (weight1 * weight2)
   cosine
   }
 }


}
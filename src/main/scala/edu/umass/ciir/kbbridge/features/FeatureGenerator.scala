package edu.umass.ciir.kbbridge.features

trait FeatureGenerator {
  def addFeature(prefix:String, category:String, value:String)
  def addValueFeature(prefix:String, category:String, value:Double)
}

trait BinaryOnlyFeatures extends FeatureGenerator {
  def addValueFeature(prefix: String, category: String, value: Double) = {
    addFeature(prefix, category, ""+value)
  }
}

case class InvalidFeatureValueException(msg:String) extends RuntimeException(msg)
class FeatureSetup(addFeatureCall:(String, String, String) => Unit,  addFeatureValueCall:(String,String,Double) => Unit) extends FeatureGenerator{
  def addFeature(prefix: String, category: String, value: String) = addFeatureCall(prefix, category, value)

  def addValueFeature(prefix: String, category: String, value: Double) = {
    if(value.isNaN) throw InvalidFeatureValueException("Feature Value is NaN. prefix="+prefix+" category="+category+" value="+value)
    if(value.isInfinite) throw InvalidFeatureValueException("Feature Value is Infinite. prefix="+prefix+" category="+category+" value="+value)
    addFeatureValueCall(prefix, category, value)
  }
}









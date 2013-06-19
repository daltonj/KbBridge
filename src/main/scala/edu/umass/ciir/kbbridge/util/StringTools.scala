package edu.umass.ciir.kbbridge.util


object StringTools {
  def getTabSplits(line:String, min:Int):Seq[String] = {
    val chunks = line.split("\t")
    if (chunks.length<min) throw new Error ("Line "+line+" was expected to have "+min+" tab separated entries but has only "+chunks.length+". line="+line)
    chunks
  }
  def getSepSplits(line:String, sep:String="\\s+",min:Int = -1):Seq[String] = {
    val chunks = line.split(sep)
    if (chunks.length<min) throw new Error ("Line "+line+" was expected to have "+min+" tab separated entries but has only "+chunks.length+". line="+line)
    chunks
  }

  def getSplits(line:String):Seq[String] = {
    val chunks = line.split("\\s+")
    chunks
  }

  def getSplitChunk(line:String,idx:Int):Option[String] = {
    val chunks = getSplits(line)
    if (chunks.length<idx+1)
      None
    else
      Some(chunks(idx))
  }

  def readFileContents(filename: String, sepStr:String = " "): String = {
    val source = io.Source.fromFile(filename)
    try{
      val content = source.getLines().mkString(sepStr)
      content
    } finally {
      source.close()
    }
  }

  def toIntOption(str:String):Option[Int] = {
    if (str.isEmpty) None
    else if( str.head.isDigit ) scala.Some(str.toInt)
    else None
  }

  def zapParentheses(str:String, beginZap:Char = '(', endZap:Char = ')'):String = {
    var delete:Int = 0
    val filteredCharArray =
      for (c <- str) yield {
        if (c == beginZap) delete += 1
        val result =
          if(delete == 0) Some(c)
          else None
        if (c == endZap) delete -= 1
        result
    }
    filteredCharArray.flatten.mkString
  }
}

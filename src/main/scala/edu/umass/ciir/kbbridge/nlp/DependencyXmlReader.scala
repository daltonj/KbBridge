package edu.umass.ciir.kbbridge.nlp

import java.io.File
import collection.mutable.ListBuffer
import xml.{Node, XML}
import org.xml.sax.SAXParseException

/**
 * 
 */

object DependencyXmlReader {
  case class Vertex(idx:Int, word:String, children: ListBuffer[Vertex] = ListBuffer(), parents: ListBuffer[Vertex] = ListBuffer()){
    def isRoot = parents.isEmpty
    def isLeaf = children.isEmpty
    override def toString = {
      idx+":"+word+" ["+children.mkString(",")+"]"
    }
  }
  def getBasicDependenyTrees(nlpFile:File):Seq[Seq[Vertex]] = {
    val xmlReader = new java.io.InputStreamReader(new java.io.FileInputStream(nlpFile), "ISO-8859-1")
    try {
      for(sentences <- (XML.load(reader = xmlReader) \\ "sentences"); s <- sentences \\ "sentence")  yield {

        val vertices = scala.collection.mutable.Map[Int,  Vertex]()
        for((dep:Node) <- s \ "basic-dependencies" \\ "dep"){
          val gidx = (dep  \ "governor" \ "@idx").text.toInt
          val govWord = (dep \ "governor").text


          val didx = ((dep:Node) \ "dependent" \ "@idx").text.toInt
          val depWord = (dep \ "dependent").text


          val governor = vertices.getOrElseUpdate(gidx, {new Vertex(gidx, govWord)})
          val dependent = vertices.getOrElseUpdate(didx, {new Vertex(didx, depWord)})

          governor.children += dependent
          dependent.parents += governor
        }
        vertices.values.toSeq.sortBy(v => v.idx)
      }

    } catch {
      case ex:SAXParseException => {
        System.err.println("Sax exception when parsing NERs from "+nlpFile.getAbsolutePath)
        throw ex
      }
    } finally {
      xmlReader.close()
    }
  }
}

object DependencyXmlReaderTest {
  def main(args:Array[String]) {
    val trees = DependencyXmlReader.getBasicDependenyTrees(new File("/Users/dietz/tac-kbp2011/data/extractStanf/eng-WL-11-174584-12960463.xml"))

    for(tree <- trees){
      println("sentence: ")
      for(v <- tree) {
        println(v.idx +":"+v.word)
      }

      for(root <- tree.filter(_.isRoot)){
        println(root)
      }
      println
    }


  }
}
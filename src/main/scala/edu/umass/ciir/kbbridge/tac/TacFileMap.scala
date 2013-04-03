package edu.umass.ciir.kbbridge.tac

import collection.mutable.HashMap
import java.io.File
import java.io.PrintWriter
import edu.umass.ciir.kbbridge.util.ConfInfo

object TacFileMap {
    
    private val docIdToFilename = new HashMap[String,  String]()
    val filePath = ConfInfo.sourceDir + "docIdMap.txt"
    println("Loading file list from " + filePath)
    private val f = io.Source.fromFile(filePath)
    for(line <- f.getLines()){
      val sp = line.split("\t")
      docIdToFilename += sp(0) -> sp(1)
    }
    f.close
    println("doc id to full text map loaded with: " + docIdToFilename.size + " entries")
    
    
    def addFileToMap(file : File) {
      if (file.isDirectory()) {
        val subFiles = file.listFiles()
        subFiles.map(s => addFileToMap(s))
      } else {
        val extensionIdx = file.getName().lastIndexOf(".")
        val docId = file.getName().substring(0,extensionIdx)
        val relativePath = file.getAbsolutePath().replace(ConfInfo.sourceDir,"")
        //println("Adding file to map: " + docId + " " + "path: " + relativePath )
        docIdToFilename += ( docId -> relativePath)
      }
      
    }

    val docIdToFilenameMap = docIdToFilename.result()
    
    def main(args: Array[String]) {
      val outputFile = "./" + "docIdMap.txt"
      println("Starting to write id file to: " + outputFile)
      val p = new PrintWriter( outputFile, "UTF-8")
      addFileToMap(new File(ConfInfo.sourceDir))
      val map = TacFileMap.docIdToFilenameMap
      println("doc id to full text map loaded with: " + map.size + " entries")
      for (k <- map.keys) {
        p.println(k + "\t" + map(k))
      }
      p.close()
    }
}
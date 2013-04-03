package edu.umass.ciir.kbbridge.data

import edu.umass.ciir.kbbridge.nlp.TextNormalizer
import edu.umass.ciir.kbbridge.search.KnowledgeBaseSearcher
import edu.umass.ciir.kbbridge.util.WikiXmlTextExtractor

/**
 * User: jdalton
 * Date: 3/29/13
 */
object WikipediaDocumentSource extends DocumentTextSource {

  override def fullText (docId:String) {
    val searcher = KnowledgeBaseSearcher.getSearcher()
    val document = searcher.getDocument(docId, true).document
    val text = TextNormalizer.normalizeText(WikiXmlTextExtractor.extractText(document))
    text
  }

}

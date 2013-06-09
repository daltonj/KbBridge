package edu.umass.ciir.kbbridge.search;

import edu.umass.ciir.memindex.Query;
import org.lemurproject.galago.core.index.AggregateReader;
import org.lemurproject.galago.core.scoring.WeightedTerm;
import org.lemurproject.galago.core.tools.Search;

import java.util.List;
import java.util.Map;

/**
 * User: dietz
 * Date: 6/7/13
 * Time: 12:11 PM
 */
public interface KnowledgeBaseSearcherInterface {
    Search.SearchResultItem[] search(Query query, List<String> coreferentStrings, List<String> nerNeighbors) throws Exception;

    Search.SearchResultItem[] search(Query query, List<String> coreferentStrings, List<String> nerNeighbors, List<String> sentences) throws Exception;

//    Search.SearchResultItem[] searchRm(Query query, List<String> coreferentStrings, List<WeightedTerm> rmTerms, List<WeightedTerm> nerNeighbors, List<Search.SearchResultItem> firstPassResults, List<String> sentences, boolean useWorkingSet) throws Exception;

//    Search.SearchResultItem[] searchSnd(Query query, List<String> coreferentStrings, Map<String, Double> nerNeighbors, List<Search.SearchResultItem> firstPassResults, List<String> sentences) throws Exception;

    Search.SearchResultItem[] searchComponents(Query query,
                                               List<String> nameVariants,
                                               Map<String, Double> nerNeighbors,
                                               List<Search.SearchResultItem>
                                                       firstPassResults,
                                               List<String> sentences,
                                               Map<String, Double> uniformNer,
                                               Map<String, Double> localNer,
                                               Map<String, Double> discountNer,
                                               Map<String, Double> discountAdd)

    throws Exception;
//
//    Search.SearchResultItem[] searchType(Query query,
//                                         String type,
//                                         List<String> coreferentStrings,
//                                         List<String> contextStrings) throws Exception;

    Search.SearchResultItem[] search(Query query) throws Exception;

    Search.SearchResultItem getDocument(String docId, boolean getTerms) throws Exception;

//    AggregateReader.NodeStatistics getStatistics(String query) throws Exception;

//    long numDocumentsInCollection() throws Exception;
//
//    long totalCollectionFrequency() throws Exception;

    long getFieldTermCount(String term, String field) throws Exception;
}

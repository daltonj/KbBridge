package edu.umass.ciir.kbbridge.search;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import edu.umass.ciir.kbbridge.util.ConfInfo;
import edu.umass.ciir.memindex.Query;
import edu.umass.ciir.models.StopWordList;
import org.lemurproject.galago.core.index.AggregateReader;
import org.lemurproject.galago.core.index.AggregateReader.NodeStatistics;
import org.lemurproject.galago.core.parse.Document;
import org.lemurproject.galago.core.retrieval.Retrieval;
import org.lemurproject.galago.core.retrieval.query.Node;
import org.lemurproject.galago.core.retrieval.query.StructuredQuery;
import org.lemurproject.galago.core.scoring.WeightedTerm;
import org.lemurproject.galago.core.tools.Search;
import org.lemurproject.galago.core.tools.Search.SearchResult;
import org.lemurproject.galago.core.tools.Search.SearchResultItem;
import org.lemurproject.galago.tupleflow.Parameters;
import scala.actors.threadpool.Arrays;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

public class KnowledgeBaseSearcher {

    Parameters queryParams = new Parameters();
    Search m_searcher;

    private final Pattern commaMatch = Pattern.compile(",");
    private final Pattern spaceMatch = Pattern.compile(" ");

    private Double defaultSmoothingMu;

    PrintWriter debugWriter = new PrintWriter("debug-file");
    
    private static PrintWriter queryWriter;

    private final boolean useLocalIndex;
    private final String candidateQueryType;
    private Parameters globalParameters;

    private Long numDocsInCollection;
    private Long collectionTermFrequency;

    private static HashMap<String, KnowledgeBaseSearcher> searcherMap = new HashMap<String, KnowledgeBaseSearcher>();

    LoadingCache<String, NodeStatistics> termStatisticsCache = CacheBuilder.newBuilder()
            .maximumSize(1000)
            .expireAfterWrite(10, TimeUnit.MINUTES)
            .build(
                    new CacheLoader<String, NodeStatistics>() {
                        public NodeStatistics load(String key) throws Exception {
                            return getStatistics(key);
                        }
                    });

    public static KnowledgeBaseSearcher getSearcher() throws Exception {
        return getSearcher("default");
    }

    public static KnowledgeBaseSearcher getSearcher(String searcherName) throws Exception {
        KnowledgeBaseSearcher kbSearcher = searcherMap.get(searcherName);
        if (kbSearcher == null) {
            kbSearcher = new KnowledgeBaseSearcher(ConfInfo.galagoJsonParameterFile,ConfInfo.galagoUseLocalIndex, ConfInfo.galagoSrv, ConfInfo.galagoPort, ConfInfo.candidateQueryType, ConfInfo.kbSearcherResultLogFile);
            searcherMap.put(searcherName, kbSearcher);
        }
        return kbSearcher;
    }

    public static KnowledgeBaseSearcher getSearcher(String searcherName, String jsonConfigFile, boolean galagoUseLocalIndex, String galagoSrv, String galagoPort, String candidateQueryType, String resultLogFileName) throws Exception {
        KnowledgeBaseSearcher kbSearcher = searcherMap.get(searcherName);
        if (kbSearcher == null) {
            kbSearcher = new KnowledgeBaseSearcher(jsonConfigFile,galagoUseLocalIndex, galagoSrv, galagoPort, candidateQueryType, resultLogFileName);
            searcherMap.put(searcherName, kbSearcher);
        }
        return kbSearcher;
    }

    public Search getUnderlyingSearcher() {
        return m_searcher;
    }

    private KnowledgeBaseSearcher(String jsonConfigFile, boolean galagoUseLocalIndex, String galagoSrv, String galagoPort, String candidateQueryType, String resultLogFileName) throws Exception {
        useLocalIndex = galagoUseLocalIndex;
        this.candidateQueryType = candidateQueryType;

        globalParameters = new Parameters();

        if (useLocalIndex) {
            // String jsonConfigFile = ConfInfo.galagoJsonParameterFile;
            // read the galago json file into parameters directly
            globalParameters = Parameters.parse(new File(jsonConfigFile));
            defaultSmoothingMu = globalParameters.getDouble("defaultSmoothingMu");

            //            File indexFile = new File(globalParameters.getString("index"));
            //            getQueryWriter(resultLogFileName + "_" + indexFile.getName());

        } else {
            // LD: even in client/server mode, the client needs to access the fields, stored in the parameters.
            Parameters jsonparams = Parameters.parse(new File(jsonConfigFile));
            defaultSmoothingMu = jsonparams.getDouble("defaultSmoothingMu");
            String remoteIndex = "http://"+ galagoSrv+":"+galagoPort;
            globalParameters.set("index", remoteIndex);
            getQueryWriter(galagoSrv + "_" + galagoPort);

        }

        if (globalParameters.isString("index")) System.out.println("** Loading index from: " + globalParameters.getString("index"));

        m_searcher = new Search(globalParameters);

                String lengthString = "#lengths:document:part=lengths()";
                Node n = StructuredQuery.parse(lengthString);

              AggregateReader.CollectionStatistics collectionStats = m_searcher.getRetrieval().getCollectionStatistics(n);
              numDocsInCollection =collectionStats.documentCount;
              collectionTermFrequency = collectionStats.collectionLength;

       // numDocsInCollection = m_searcher.getRetrievalStats().documentCount;
      //  collectionTermFrequency = m_searcher.getRetrievalStats().collectionLength;


    }

    private synchronized void getQueryWriter(String key) throws Exception {

        File file = new File(key + "_query_params.queries");
        if (!file.exists()) {
            queryWriter = new PrintWriter(file);
            queryWriter.println("{\"requested\" : 100 ,");
            queryWriter.println("\"queries\" : [");
        }
    }

    public SearchResultItem[] search(Query query, List<String> coreferentStrings, List<String> nerNeighbors) throws Exception {
        Parameters p = new Parameters();
        //        p.set("indexId", id);
        //        p.set("queryType", qtype);
        p.set("startAt", 0);
        p.set("resultCount", query.getNumResults());
        p.set("retrievalGroup", "all");
        p.set("requested", query.getNumResults());
        p.set("mu", defaultSmoothingMu);
        p.set("odw",0.21D);
        p.set("uniw", 0.29D);
        p.set("uww", 0.50D);

        List<String> tokens = normalize(query.getRawQuery());
        StringBuilder sb = new StringBuilder();

        buildBasicQuery(coreferentStrings, tokens, sb);

        System.out.println(sb.toString());
        if (queryWriter != null) {
            queryWriter.println(String.format("{\"number\":\"%s\", \"text\":\"%s\"},", query.getQueryNum(), sb.toString()));
        }
        return localSearch(p, sb.toString() , 5);

    }

    public SearchResultItem[] search(Query query, List<String> coreferentStrings, List<String> nerNeighbors, List<String> sentences) throws Exception {
        Parameters p = new Parameters();
        //        p.set("indexId", id);
        //        p.set("queryType", qtype);
        p.set("startAt", 0);
        p.set("resultCount", query.getNumResults());
        p.set("retrievalGroup", "all");
        p.set("requested", query.getNumResults());
        p.set("mu", defaultSmoothingMu);
        p.set("odw",0.21D);
        p.set("uniw", 0.29D);
        p.set("uww", 0.50D);

        List<String> tokens = normalize(query.getRawQuery());
        StringBuilder sb = new StringBuilder();

        System.out.println("Number of sentences: " + sentences.size());

        if (sentences.size() == 0) {
            buildBasicQuery(coreferentStrings, tokens, sb);

        } else {

            sb.append("#combine:0=0.7:1=0.2:2=0.1( ");
            buildBasicQuery(coreferentStrings, tokens, sb);

            sb.append(" #combine");
            sb.append("( ");
            for (String sent : sentences) {

                String truncated = sent;
                if (sent.length() > 200) {
                    truncated = sent.substring(0, Math.min(200, sent.indexOf(" ", 180)));
                }
                buildSeqDepForString(sb, truncated);
                //buildTermForString(sb, sent);
            }
            sb.append(" )");

            if(!nerNeighbors.isEmpty()){

                sb.append(" #combine");
                int i = 0;
                for (String ner : nerNeighbors) {
                    double weight = 1.0; //ner.getWeight();
                    if (checkString(ner)) {
                        sb.append(":"+i+"="+Double.toString(weight));
                        i++;
                    }
                }


                sb.append("(");
                for (String ner : nerNeighbors) {
                    if (checkString(ner)) {
                        buildSeqDepForString(sb, ner);
                    }
                }
                sb.append(" )");
            }

        }
        System.out.println(sb.toString());
        if (queryWriter != null) {
            queryWriter.println(String.format("{\"number\":\"%s\", \"text\":\"%s\"},", query.getQueryNum(), sb.toString()));
        }
        return localSearch(p, sb.toString() , 5);

    }

    private void buildBasicQuery(List<String> coreferentStrings, List<String> tokens, StringBuilder sb) {
        sb.append("#combine(#seqdep ( ");
        for (String token : tokens) {
            sb.append(token);
            sb.append(" ");
        }
        sb.append(")");

        boolean useContext = true;

        if (useContext && coreferentStrings.size() > 0) {
            sb.append(" #combine(");

            for (String corefString: coreferentStrings) {

                buildSeqDepForString(sb, corefString);
            }
            sb.append(")");

        }
        sb.append(")");
    }

    private void buildSeqDepForString(StringBuilder sb, String string) {
        Iterable<String> tokens = normalize(string);

        sb.append(" #seqdep(" );
        for (String token : tokens) {
            if (!StopWordList.isStopWord(token)) {
                sb.append(token);
                sb.append(" ");
            }
        }
        sb.append(") ");
    }

    private boolean checkString(String string) {
        Iterable<String> tokens = normalize(string);

        boolean result = false;
        for (String token : tokens) {
            if (!StopWordList.isStopWord(token)) {
                result = true;
            }
        }
        return result;
    }

    private void buildTermForString(StringBuilder sb, String string) {
        Iterable<String> tokens = normalize(string);

        //        sb.append(" #combine(" );
        for (String token : tokens) {
            if (!StopWordList.isStopWord(token)) {
                sb.append(token);
                sb.append(" ");
            }
        }
        //        sb.append(") ");
    }

    private void cleanTokens(StringBuilder sb, String string) {
        Iterable<String> tokens = normalize(string);

        //        sb.append(" #combine(" );
        for (String token : tokens) {
            if (!StopWordList.isStopWord(token)) {
                sb.append(token);
                sb.append(" ");
            }
        }
        //        sb.append(") ");
    }

    public SearchResultItem[] searchRm(Query query, List<String> coreferentStrings, List<WeightedTerm> rmTerms, List<WeightedTerm> nerNeighbors, List<SearchResultItem> firstPassResults, List<String> sentences, boolean useWorkingSet) throws Exception {
        List<String> firstPassDocIds = new ArrayList<String>();
        for(SearchResultItem item:firstPassResults){
            firstPassDocIds.add(item.identifier);
            //System.out.println("workset docid = " + item.identifier);
        }

        Parameters p = new Parameters();
        //        p.set("indexId", id);
        //        p.set("queryType", qtype);
        p.set("startAt", 0);
        p.set("resultCount", query.getNumResults());
        p.set("retrievalGroup", "all");
        p.set("requested", query.getNumResults());
        p.set("mu", defaultSmoothingMu);
        p.set("odw",0.21D);
        p.set("uniw", 0.29D);
        p.set("uww", 0.50D);

        if(useWorkingSet && firstPassDocIds.size() > 0){
            p.set("working", firstPassDocIds);
            System.out.println("using working set with " + firstPassDocIds.size() + " docs");
        }

        List<String> tokens = normalize(query.getRawQuery());
        StringBuilder sb = new StringBuilder();


        sb.append("#combine:0=0.6:1=0.2:2=0.2(");

        buildBasicQuery(coreferentStrings, tokens, sb);

        if (rmTerms.size() > 0) {
            sb.append(" #combine");
            int k=0;
            for (WeightedTerm t : rmTerms) {
                double weight = t.getWeight();
                if (checkString(t.getTerm())) {
                    sb.append(":"+k+"="+Double.toString(weight));
                    k++;
                }
            }
            sb.append("(");
            StringBuilder allTerms = new StringBuilder();
            for (WeightedTerm t : rmTerms) {
                allTerms.append(t.getTerm());
                allTerms.append(" ");
            }
            cleanTokens(sb, allTerms.toString());
            sb.append(") ");
        }

        if(!nerNeighbors.isEmpty()){

            sb.append(" #combine");
            int i = 0;
            for (WeightedTerm ner : nerNeighbors) {
                double weight = ner.getWeight();
                if (checkString(ner.getTerm())) {
                    sb.append(":"+i+"="+Double.toString(weight));
                    i++;
                }
            }


            sb.append("(");
            for (WeightedTerm ner : nerNeighbors) {
                if (checkString(ner.getTerm())) {
                    buildSeqDepForString(sb, ner.getTerm());
                }
            }
            sb.append(" )");
        }

        //        if(!sentences.isEmpty()){
        //
        //            sb.append(" #combine");
        //            sb.append("( ");
        //            for (String sent : sentences) {
        //                //            buildSeqDepForString(sb, sent);
        //                if (checkString(sent)) {
        //                    buildTermForString(sb, sent);
        //                }
        //            }
        //
        //            sb.append(" )");
        //        }
        sb.append(" )");



        System.out.println(sb.toString());
        if (queryWriter != null) {
            queryWriter.println(String.format("{\"number\":\"%s\", \"text\":\"%s\"},", query.getQueryNum(), sb.toString()));
        }
        return localSearch(p, sb.toString() , 5);

    }

    public SearchResultItem[] searchSnd(Query query, List<String> coreferentStrings, Map<String,Double> nerNeighbors, List<SearchResultItem> firstPassResults, List<String> sentences) throws Exception {
        List<String> firstPassDocIds = new ArrayList<String>();
        for(SearchResultItem item:firstPassResults){
            firstPassDocIds.add(item.identifier);
            System.out.println("workset docid = " + item.identifier);
        }

        Parameters p = new Parameters();
        //        p.set("indexId", id);
        //        p.set("queryType", qtype);
        p.set("startAt", 0);
        p.set("resultCount", query.getNumResults());
        p.set("retrievalGroup", "all");
        p.set("requested", query.getNumResults());
        p.set("mu", defaultSmoothingMu);
        p.set("odw",0.21D);
        p.set("uniw", 0.29D);
        p.set("uww", 0.50D);

        if(!ConfInfo.noFirstPassQuery){
            p.set("working", firstPassDocIds);
        }

        List<String> tokens = normalize(query.getRawQuery());
        StringBuilder sb = new StringBuilder();


        sb.append("#combine:0=0.7:1=0.2:2=0.1(");

        buildBasicQuery(coreferentStrings, tokens, sb);


        sb.append(" #combine");
        int i = 0;
        for (String ner : nerNeighbors.keySet()) {
            double weight = nerNeighbors.get(ner);
            if (checkString(ner)) {
                sb.append(":"+i+"="+weight);
                i++;
            }
        }


        sb.append("(");
        for (String ner : nerNeighbors.keySet()) {
            if (checkString(ner)) {
                buildSeqDepForString(sb, ner);
            }
        }
        sb.append(" )");

        if(!sentences.isEmpty()){

            sb.append(" #combine");
            sb.append("( ");
            for (String sent : sentences) {
                //            
                if (checkString(sent)) {
                    //buildTermForString(sb, sent);
                    String truncated = sent;
                    if (sent.length() > 200) {
                        truncated = sent.substring(0, Math.min(200, sent.indexOf(" ", 180)));
                    }
                    buildSeqDepForString(sb, truncated);
                }
            }

            sb.append(" )");
        }
        sb.append(" )");



        System.out.println(sb.toString());
        if (queryWriter != null) {
            queryWriter.println(String.format("{\"number\":\"%s\", \"text\":\"%s\"},", query.getQueryNum(), sb.toString()));
        }
        return localSearch(p, sb.toString() , 5);

    }


    public SearchResultItem[] searchComponents(Query query, 
            List<String> nameVariants, 
            Map<String,Double> nerNeighbors, 
            List<SearchResultItem> 
            firstPassResults, 
            List<String> sentences,
            Map<String,Double> uniformNer,
            Map<String,Double> localNer,
            Map<String,Double> discountNer,
            Map<String,Double> discountAdd) 
    
    throws Exception {
        List<String> firstPassDocIds = new ArrayList<String>();
        for(SearchResultItem item:firstPassResults){
            firstPassDocIds.add(item.identifier);
            System.out.println("workset docid = " + item.identifier);
        }

        Parameters p = new Parameters();
        //        p.set("indexId", id);
        //        p.set("queryType", qtype);
        p.set("startAt", 0);
        p.set("resultCount", query.getNumResults());
        p.set("retrievalGroup", "all");
        p.set("requested", query.getNumResults());
        p.set("mu", defaultSmoothingMu);
        p.set("odw",0.21D);
        p.set("uniw", 0.29D);
        p.set("uww", 0.50D);

        if(!ConfInfo.noFirstPassQuery){
            p.set("working", firstPassDocIds);
        }

        File componentOutputDir = new File("score_components");
        if (!componentOutputDir.exists()) {
            componentOutputDir.mkdirs();
        }
        File file = new File(componentOutputDir.getAbsoluteFile() + File.separator + query.getQueryNum());
        System.out.println("Starting to write to file: " + file.getAbsolutePath());
        PrintWriter componentWriter = new PrintWriter(file);

        // ORIGINAL "raw" query
        StringBuilder rawSb = new StringBuilder();
        buildSeqDepForString(rawSb,query.getRawQuery());
        SearchResultItem[] originalResults = localSearch(p, rawSb.toString() , 5);
        serializeComponent("raw", query, originalResults, componentWriter);

        //NAME VARIANTS expansion term
        StringBuilder nvSb = new StringBuilder();
        nvSb.append(" #combine(");
        for (String name: nameVariants) {
            if (checkString(name)) {
                buildSeqDepForString(nvSb, name);
            }
        }
        nvSb.append(")");
        SearchResultItem[] nvResults = localSearch(p, nvSb.toString() , 5);
        serializeComponent("nv", query, nvResults, componentWriter);

        // NER neighborhood
        String nerQuery = constructedWeightedPhraseQuery(query, p, ConfInfo.nerNeighborQueryMethod+"_ner", nerNeighbors, componentWriter, false);
        
        String nerDescription = "";
        if (ConfInfo.nerNeighborQuerySelectMethod.equals("all")){
            nerDescription = "all";
        } else if (ConfInfo.nerNeighborQuerySelectMethod.equals("kclosest")) {
            nerDescription = "kclosest_" + ConfInfo.nerNeighborQueryK;
        } else {
            throw new Exception("Unsupported neighbor type");
        }
        constructedWeightedPhraseQuery(query, p, nerDescription + "_uniform_ner", uniformNer, componentWriter, true);
        constructedWeightedPhraseQuery(query, p, nerDescription + "_local_ner", localNer, componentWriter, true);
        constructedWeightedPhraseQuery(query, p, nerDescription + "_prfweight_ner", discountNer, componentWriter, true);
        constructedWeightedPhraseQuery(query, p, nerDescription + "_prfentites_ner", discountAdd, componentWriter, true);
        
        
        // SENTENCES
        StringBuilder sentSb = new StringBuilder();
        if (!sentences.isEmpty()) {
            sentSb.append(" #combine");
            sentSb.append("( ");
            for (String sent : sentences) {          
                if (checkString(sent)) {
                    String truncated = sent;
                    if (sent.length() > 200) {
                        truncated = sent.substring(0, Math.min(200, sent.indexOf(" ", 180)));
                    }
                    buildSeqDepForString(sentSb, truncated);
                }
            }

            sentSb.append(" )");
            SearchResultItem[] sentResults = localSearch(p, sentSb.toString() , 5);
            serializeComponent("sent", query, sentResults, componentWriter);
        }
        
        // ALL combined
        StringBuilder allCombined = new StringBuilder();
        allCombined.append("#combine:0=0.35:1=0.35:2=0.2:3=0.1(");
        allCombined.append(rawSb.toString() + " " + nvSb.toString() + " " + nerQuery + " " + sentSb.toString());
        allCombined.append(")");
        SearchResultItem[] allResults = localSearch(p, allCombined.toString() , 5);
        serializeComponent("combined", query, allResults, componentWriter);
        componentWriter.flush();
        componentWriter.close();
        
        return allResults;

    }

    private String constructedWeightedPhraseQuery(Query query, Parameters p, String description, Map<String,Double> weightedPhrases, PrintWriter pw, boolean writeFeatures) 
    throws Exception {
        StringBuilder nerSb = new StringBuilder();
        nerSb.append(" #combine");
        int i = 0;
        // weights
        for (String ner : weightedPhrases.keySet()) {
            double weight = weightedPhrases.get(ner);
            if (checkString(ner)) {
                nerSb.append(":"+i+"="+weight);
                i++;
            }
        }
        // add ner term text as SDs
        nerSb.append("(");
        for (String ner : weightedPhrases.keySet()) {
            if (checkString(ner)) {
                buildSeqDepForString(nerSb, ner);
            }
        }
        nerSb.append(" )");
        if (writeFeatures) {
        SearchResultItem[] nerResults = localSearch(p, nerSb.toString() , 5);
        serializeComponent(description, query, nerResults, pw);
        }
        return nerSb.toString();
    }

    private void serializeComponent(String componentName, Query query, SearchResultItem[] results, PrintWriter pw) {

        for (SearchResultItem r : results) {
            String formatted = String.format("%s\t%s\t%s\t%s", componentName, query.getQueryNum(), r.identifier, r.score);
            pw.println(formatted);
        }

    }

    public SearchResultItem[] searchType(Query query, 
            String type,
            List<String> coreferentStrings,
            List<String> contextStrings) throws Exception {



        Parameters p = new Parameters();
        //        p.set("indexId", id);
        //        p.set("queryType", qtype);
        p.set("startAt", 0);
        p.set("resultCount", query.getNumResults());
        p.set("retrievalGroup", "all");
        p.set("requested", query.getNumResults());
        p.set("mu", defaultSmoothingMu);
        p.set("odw",0.21D);
        p.set("uniw", 0.29D);
        p.set("uww", 0.50D);

        List<String> tokens = normalize(query.getRawQuery());
        StringBuilder sb = new StringBuilder();

        String types = null;
        if (type != null) {
            types = typeToCategories(type);
            if (types != null) {
                sb.append("#require(#any( " + types + " )");
            }
        }

        sb.append(" #combine(#seqdep ( ");

        for (String token : tokens) {
            sb.append(token);
            sb.append(" ");
        }
        sb.append(")");

        boolean useContext = true;

        if (useContext && coreferentStrings.size() > 0) {
            sb.append(" #combine(");

            for (String corefString: coreferentStrings) {
                buildSeqDepForString(sb, corefString);
                //                String[] corefTokens = corefString.split("\\s+");
                //
                //                sb.append(" #seqdep(" + corefString.toLowerCase() +") ");
            }
            sb.append(")");

        }
        sb.append(")");
        if (types != null) {
            sb.append(")");
        }

        sb.append("\n");
        System.out.println(sb.toString());
        if (queryWriter != null) {
            queryWriter.println(String.format("{\"number\":\"%s\", \"text\":\"%s\"},", query.getQueryNum(), sb.toString()));
        }
        return localSearch(p, sb.toString() , 5);

    }

    private String typeToCategories(String type) {
        if (type.equals("PERSON") || type.equals("PER")) {
            return " \"/people/person\".fbtype \"Category:People\".category";
        } else if (type.equals("ORGANIZATION") || type.equals("ORG")) {
            return "\"/business/employer\".fbtype" 
                    + "\"/organization/organization\".fbtype" + 
                    "\"/government/governmental_body\".fbtype" +
                    "\"/book/newspaper\".fbtype" +
                    "\"/base/newsevents/news_reporting_organisation\".fbtype" +
                    "\"/soccer/football_league\".fbtype" +
                    "\"/organization/organization\".fbtype" +
                    "\"/organization/organization\".fbtype";
        } else if (type.equals("LOCATION") || type.equals("GPE")) {
            return " \"/location/location\".fbtype";  
        } else if (type.equals("UNK")) {
            return null;
        } else {
            throw new IllegalArgumentException("Invalid type category!: type: " + type);
        }
    }

    public SearchResultItem[] search(Query query) throws Exception {
        Parameters p = new Parameters();
        //        p.set("indexId", id);
        //        p.set("queryType", qtype);
        p.set("startAt", 0);
        p.set("resultCount", query.getNumResults());
        p.set("retrievalGroup", "all");
        p.set("requested", query.getNumResults());
        //        p.set("caching", ConfInfo.cacheQueries);
        p.set("mu", defaultSmoothingMu);
        p.set("odw",0.21D);
        p.set("uniw", 0.29D);
        p.set("uww", 0.50D);

        String rawQueryText = query.getRawQuery();
        String queryToRun;
        if("od".equalsIgnoreCase(candidateQueryType)){
            queryToRun  = generateOdQuery(normalize(rawQueryText));
        } else if("prms".equalsIgnoreCase(candidateQueryType)){
            queryToRun = generateQueryPRMS(normalize(rawQueryText));
        } else if ("exact".equalsIgnoreCase(candidateQueryType)){
            queryToRun = generateQuotedQuery(normalize(rawQueryText));
        } else if ("combine".equalsIgnoreCase(candidateQueryType)) {
            queryToRun = generateSimpleQuery(normalize(rawQueryText), "combine");
        } else if ("seqdep".equalsIgnoreCase(candidateQueryType)) {
            queryToRun = generateSimpleQuery(normalize(rawQueryText), "seqdep");
        }  else if ("rm".equalsIgnoreCase(candidateQueryType)) {
            queryToRun = generateRFQuery(normalize(rawQueryText), "seqdep");
        } else {
            queryToRun = generateQuery(normalize(rawQueryText));
        }

        //   [{"number":"id", "text":"query text"}, ...]
        if (queryWriter != null) {
            queryWriter.println(String.format("{\"number\":\"%s\", \"text\":\"%s\"},", query.getQueryNum(), queryToRun));
        }
        return localSearch(p, queryToRun , 5);
    }

    private SearchResultItem[]  localSearch(Parameters p, String queryToRun, int tries) throws Exception {
        try{
            System.out.println("Running query:" + queryToRun );
            //System.out.println("Parameters: " + p.toPrettyString());
            debugWriter.println(queryToRun);
            synchronized (m_searcher){

                Node root = StructuredQuery.parse(queryToRun);
                // System.out.println("Running parsed query: " + root.toString());
                Node transformed =  m_searcher.getRetrieval().transformQuery(root, p);
                //  System.out.println("Running transformed query: " + transformed.toPrettyString());
                SearchResult result = m_searcher.runTransformedQuery(transformed, p, false);

                List<SearchResultItem> results = result.items;
                for (SearchResultItem r : results) {
                    String formatted = String.format("%s\t%s\t%s", r.rank, r.identifier, r.score);
                    debugWriter.println(formatted);
                }
                debugWriter.flush();

                return results.toArray(new SearchResultItem[0]);
            }
        } catch (NullPointerException ex) {
            System.out.println("NPE while running query "+queryToRun);
            throw ex;
        } catch (IOException ex) {
            if(tries > 0){
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {

                }
                return localSearch(p, queryToRun, tries -1);
            } else {
                throw ex;
            }
        }
    }


    public SearchResultItem getDocument(String docId, boolean getTerms) throws Exception {
        //        throw new UnsupportedOperationException("fix galago");
        System.out.println("fetching doc: " + docId);
        synchronized (m_searcher) {

            Parameters p1 = new Parameters();
            if (ConfInfo.galagoUseLocalIndex) {
                p1.set("terms", true);
                p1.set("tags", true);
            } else {
                if (getTerms) {
                    p1.set("terms", true);
                    p1.set("tags", true);
                } else {
                    p1.set("terms", false);
                    p1.set("tags", false);
                }
            }

            Document doc = m_searcher.getDocument(docId, p1);

            //            if (!ConfInfo.galagoUseLocalIndex) { 
            //                System.out.println("parsing doc");
            //                List<String> fields = java.util.Arrays.asList("anchor", "title", "redirect", "text", "fbname", "anchor-exact");
            //                TagTokenizer tt = new TagTokenizer();
            //                for (String field : fields) {
            //                    tt.addField(field);
            //                }
            //                tt.process(doc);
            //            }

            SearchResultItem result = null;
            if (doc != null) {
                result = new SearchResultItem();
                result.rank = -1;
                result.identifier = doc.name;
                result.displayTitle = doc.name;
                result.document = doc;
                result.metadata = doc.metadata;
            }

            return result;
        }
    }

    public NodeStatistics getStatistics(String query) throws Exception {
        System.out.println("fetching statistics: " + query);
        synchronized (m_searcher) {
            try {
                Retrieval r = m_searcher.getRetrieval();
                Node root = StructuredQuery.parse(query);
                root.getNodeParameters().set("queryType", "count");
                Node transformed = r.transformQuery(root, queryParams);
                NodeStatistics statistics = r.getNodeStatistics(transformed);
                //NodeStatistics statistics = r.nodeStatistics(transformed);

                return statistics;
            } catch (Exception e) {
                System.out.println("Error getting statistics for query: " + query);
                throw e;
            }
        }
    }

    public long numDocumentsInCollection() throws Exception {
        return numDocsInCollection;
    }

    public long totalCollectionFrequency() throws Exception {
        return collectionTermFrequency;
    }

    public double idf(String term) throws Exception {
        NodeStatistics statistics = getStatistics(generateSingleTermQuery(term));
        System.out.println(Math.log(collectionTermFrequency)+"  "+ Math.log( 1+ statistics.nodeDocumentCount));
        return Math.log(collectionTermFrequency) - Math.log( 1+ statistics.nodeDocumentCount);
    }

    public long getFieldTermCount(String term, String field) throws Exception {
        String normalized = cleanString(term.toLowerCase().trim());
        if (normalized.length() > 0) {        
            String transformedText = "\"" + normalized + "\"" +"." + field ;

            NodeStatistics statistics = termStatisticsCache.get(transformedText);
            return statistics.nodeFrequency;
        } else {
            return 0;
        }
    }

    public long getAnchorCountInDoc(String query, String docId) throws Exception {
        String transformedText = "#require( #equals( names " + docId + " ) #combine( " + query.toLowerCase() + ".anchor))" ;
        System.out.println(transformedText);
        synchronized (m_searcher) {
            NodeStatistics statistics = getStatistics (transformedText);
            return statistics.nodeFrequency;
        }
    }


    /**
     * Takes a raw query, normalizes it and turns it into a list of clean tokens.
     * 
     * @param query
     * @return
     */
    public static List<String> normalize(String query) {
        List<String> normalizedTokens = new ArrayList<String>();
        String cleanQuery = query.replace("-", " ");
        List<String> tokens = (List<String>) Arrays.asList(cleanQuery.split("\\s+"));
        for (int i=0; i < tokens.size(); i++) {
            String term = tokens.get(i);
            //	        if (StopWordList.isStopWord(term)) {
            //	            return new Query("",m_numRequestedResults);
            //	        }
            term = cleanString(term);
            term = term.toLowerCase();
            term = term.trim();
            if (term.length() > 1) {
                normalizedTokens.add(term);
            }
        }
        return normalizedTokens;
    }

    /**
     * Ensure a safe galago query term.
     * 
     * @param queryTerm
     * @return
     */
    public static String cleanString(String queryTerm) {
        return queryTerm.replaceAll("[^a-zA-Z0-9 ]", "");
    }

    private String generateSingleTermQuery(String token) {
        //		StringBuilder sb = new StringBuilder();
        //		sb.append("#combine( ");
        //		//String[] fields = {"anchor", "title", "redirects", "fbnames"};
        //		String[] fields = ConfInfo.candidateFields;
        //			for (String field : fields) {
        //				sb.append(cleanString(token));
        //				sb.append("." + field);
        //				sb.append(" ");
        //			}
        //		sb.append(")");
        //		return sb.toString();
        return ("#counts:"+token+":part=field.text");

    }
    private String generateQuery(List<String> tokens) {
        StringBuilder sb = new StringBuilder();
        sb.append("#combine( ");
        //String[] fields = {"anchor", "title", "redirects", "fbnames"};
        String[] fields = (String[]) globalParameters.getList("fields").toArray(new String[0]);
        for (String token : tokens) {
            for (String field : fields) {
                sb.append(token);
                sb.append("." + field);
                sb.append(" ");
            }
        }
        sb.append(")");
        return sb.toString();

    }
    private String generateRFQuery(List<String> tokens, String retrievalType) {
        StringBuilder sb = new StringBuilder();

        sb.append("#rm( ");
        sb.append("#" + retrievalType + "( ");
        for (String token : tokens) {
            sb.append(token);
            sb.append(" ");
        }
        sb.append(")");
        sb.append(" )");
        return sb.toString();
    }

    public static String generateSimpleQuery(List<String> tokens, String retrievalType) {
        StringBuilder sb = new StringBuilder();

        sb.append("#" + retrievalType + "( ");
        for (String token : tokens) {
            sb.append(token);
            sb.append(" ");
        }
        sb.append(")");

        return sb.toString();

    }

    private String generateQuotedQuery(List<String> tokens) {
        StringBuilder sb = new StringBuilder();
        sb.append("#combine( ");
        //String[] fields = {"anchor", "title", "redirects", "fbnames"};
        String[] fields = (String[]) globalParameters.getList("fields").toArray(new String[0]);
        for (String field : fields) {
            sb.append("\"");
            for (String token : tokens) {
                sb.append(token);
                sb.append(" ");
            }
            sb.deleteCharAt(sb.length()-1);
            sb.append("\"");
            sb.append("." + field.trim());
            sb.append(" ");
        }

        sb.append(")");
        return sb.toString();

    }



    private String generateOdQuery(List<String> tokens) {
        //#combine( #od1(george.anchor bush.anchor) #od1(george.title bush.title )
        StringBuilder sb = new StringBuilder();

        if (tokens.size() <= 1) {
            sb.append("#combine( ");
            String[] fields = (String[]) globalParameters.getList("fields").toArray(new String[0]);
            for (String field : fields) {
                for (String token : tokens) {
                    sb.append(token);
                    sb.append("." + field);
                    sb.append(" ");
                }
            }
            sb.append(")");

        } else {
            int n = tokens.size() - 1;
            sb.append("#combine( ");
            String[] fields = (String[]) globalParameters.getList("fields").toArray(new String[0]);
            for (String field : fields) {
                sb.append(" #od" + n + "( ");
                for (String token : tokens) {
                    sb.append(token);
                    sb.append("." + field);
                    sb.append(" ");
                }
                sb.append(" )");
            }
            sb.append(")");
        }
        //        System.out.println("sb.toString() = " + sb.toString());
        return sb.toString();


    }

    private String generateQueryPRMS(List<String> tokens) {
        StringBuilder sb = new StringBuilder();
        sb.append("#prms2( ");
        for (String token : tokens) {
            sb.append(token);
            sb.append(" ");
        }
        sb.append(")");
        return sb.toString();

    }

    public void close() {
        debugWriter.close();
        if (queryWriter != null) {
            queryWriter.println("]}");
            queryWriter.close();
        }
    }

    public static void main(String[] args) throws Exception {
        KnowledgeBaseSearcher searcher = new KnowledgeBaseSearcher(ConfInfo.galagoJsonParameterFile, ConfInfo.galagoUseLocalIndex, ConfInfo.galagoSrv, ConfInfo.galagoPort, ConfInfo.candidateQueryType, ConfInfo.kbSearcherResultLogFile);
        SearchResultItem[] results = searcher.search(new Query("1","california", 5));
        for (SearchResultItem r : results) {
            String formatted = String.format("%s\t%s\t%s", r.rank, r.identifier, r.score);
            System.out.println(formatted);
        }
        //        long count = searcher.getAnchorCount("california");
        //        System.out.println(count);
        //
        //        long count1 = searcher.getAnchorCountInDoc("california", "California's_16th_congressional_district");
        //        System.out.println(count1);

        searcher.close();
    }



}

package edu.umass.ciir.kbbridge.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Properties;

/**
 *
 */
public class ConfInfo {
    public static final Properties conf = ConfigLoader.readProps();

    public static final String el2009QueryFile = conf.getProperty("el2009QueryFile");
    public static final String el2009AnnoFile = conf.getProperty("el2009AnnoFile");

    public static final String el2010trainQueryFile = conf.getProperty("el2010trainQueryFile");
    public static final String el2010trainAnnoFile = conf.getProperty("el2010trainAnnoFile");

    public static final String el2010evalQueryFile = conf.getProperty("el2010evalQueryFile");
    public static final String el2010evalAnnoFile = conf.getProperty("el2010evalAnnoFile");

    public static final String el2011QueryFile = conf.getProperty("el2011TestQueryFile");
    public static final String el2011AnnoFile = conf.getProperty("el2011TestAnnoFile");

    public static final String el2012QueryFile = conf.getProperty("el2012TestQueryFile");
    public static final String el2012AnnoFile = conf.getProperty("el2012TestAnnoFile");

    public static final String sourceDir = conf.getProperty("sourceDir");

    public static final String galagoSrv = conf.getProperty("galagoSrv");
    public static final String galagoPort = conf.getProperty("galagoPort");
    public static final String galagoFullWikiPort = conf.getProperty("galagoFullWikiPort");
    public static final int maxCandidates = Integer.parseInt(conf.getProperty("candidates.maxCandidates", "10"));
    public static final int maxNuissanceCandidates = Integer.parseInt(conf.getProperty("candidates.maxNuissanceCandidates",""+maxCandidates));
    public static final int maxClosestNuissanceMentions = Integer.parseInt(conf.getProperty("neighborhood.maxClosestNuissanceMentions","-1"));
    public static final boolean restrictNuissanceMentions =  maxClosestNuissanceMentions > -1;
    public static final boolean useTacAndFullWiki = Boolean.parseBoolean(conf.getProperty("useTacAndFullWiki","false"));

    public static final String candidateQueryType =conf.getProperty("candidates.queryType","default");

    public static final int numTrainQueries =  Integer.parseInt(conf.getProperty("pipeline.numTrainQueries", "-1"));
    public static final boolean restricTrainQueries =  numTrainQueries > 0;
    public static final int numTestQueries =  Integer.parseInt(conf.getProperty("pipeline.numTestQueries", "-1"));
    public static final boolean restricTestQueries =  numTestQueries > 0;
    public static final boolean galagoUseLocalIndex = Boolean.parseBoolean(conf.getProperty("galagoUseLocalIndex", "false"));
    public static final boolean candsFromRunFile = Boolean.parseBoolean(conf.getProperty("useCachedCandidatesFromRunFile", "false"));
    public static final String candidateFileKey = conf.getProperty("candidateFileKey", "default"+"_" + ConfInfo.candidateQueryType + "_" + ConfInfo.maxCandidates + "_" + ConfInfo.numTrainQueries + "_" + ConfInfo.numTestQueries);
    public static final String neighborCandidateFileKey = conf.getProperty("neighborCandidateFileKey", "neighbor"+"_" + ConfInfo.candidateQueryType + "_" + ConfInfo.maxCandidates + "_" + ConfInfo.numTrainQueries + "_" + ConfInfo.numTestQueries);
    public static final String neighborTrainCandidateFileKey = conf.getProperty("neighborTrainCandidateFileKey", "neighborTrain"+"_" + ConfInfo.candidateQueryType + "_" + ConfInfo.maxCandidates + "_" + ConfInfo.numTrainQueries + "_" + ConfInfo.numTestQueries);
    public static final String galagoRunDir = conf.getProperty("galagoRunDir", "candidates");
    public static final String kbSearcherResultLogFile = conf.getProperty("kbSearcherResultLogFile", "default");
    public static final boolean fetchGalagoParsedDocument = Boolean.parseBoolean(conf.getProperty("fetchGalagoParsedTerms", "true"));
    
    public static final String galagoJsonParameterFile = conf.getProperty("galagoJsonParameterFile","./config/galago-fullwiki");
    public static final String galagoFullWikiJsonParameterFile = conf.getProperty("galagoFullWikiJsonParameterFile","./config/galago-fullwiki");
    public static final boolean useOracleCandidateGeneration = Boolean.parseBoolean(conf.getProperty("useOracleCandidateGeneration", "false"));
    public static final boolean filterNoNlpInfo = Boolean.parseBoolean(conf.getProperty("filterNoNlpInfo", "false"));
            
    public static final boolean createSvmInput = Boolean.parseBoolean(conf.getProperty("createSvmInput", "false"));

    public static final String[] rankingFeatures = conf.getProperty("features.ranking","nus,llcsurf").split(",");
    public static final String[] neighborLinkFeatures = conf.getProperty("features.neighborlinking","dummy").split(",");
    public static final String[] nilClassifyFeatures = conf.getProperty("features.nil","nus,llcsurf").split(",");
    public static final String[] mentionLinkClassifyFeatures = conf.getProperty("features.mentionlink","todo").split(",");
    public static final boolean normalizeFeatures = Boolean.parseBoolean(conf.getProperty("features.useNormalizedFeatures","true"));
    public static final String redisFeatureSetNameQuery2Entity = conf.getProperty("features.redisFeatureSetName", "default");
    public static final String redisFeatureSetNameEntity2Entity = conf.getProperty("features.redisFeatureSetName.entity2entity","default-e2e");
    public static final int redisFeaturePort = Integer.parseInt(conf.getProperty("features.redisFeaturePort","6380"));
    public static final String redisFeatureSvr = conf.getProperty("features.redisFeatureSvr","");
    public static final int redisFeatureTimeout = Integer.parseInt(conf.getProperty("features.redisFeatureTimeout","1000"));
    public static final int redisFeatureDB = Integer.parseInt(conf.getProperty("features.redisFeatureDB","1"));
    public static final boolean useCachedFeatures = Boolean.parseBoolean(conf.getProperty("features.useCachedFeatures","false"));
    public static final boolean useCachedFeaturesE2E = Boolean.parseBoolean(conf.getProperty("features.useCachedFeatures.e2e","false"));
    public static final boolean redisFeatureOverwriteExisting = Boolean.parseBoolean(conf.getProperty("features.redisOverwriteExisting","false"));
    public static final boolean redisFeatureOverwriteExistingE2E = Boolean.parseBoolean(conf.getProperty("features.redisOverwriteExisting.e2e","false"));



    public static final String detailedEvalOutput = conf.getProperty("eval.detailedOutput","eval.txt");
    public static final String predictionOutput = conf.getProperty("eval.predictionOutput",detailedEvalOutput.substring(0, detailedEvalOutput.lastIndexOf("."))+".tab");
    public static final String rankingOutput = conf.getProperty("eval.rankingOutput",detailedEvalOutput.substring(0, detailedEvalOutput.lastIndexOf("."))+".rank");

    public static final String idmap = conf.getProperty("idmapping","/iesl/canvas/jdalton/tac/data/tac-wiki-mapping");


    public static final String wiliLinks = conf.getProperty("wili.links","/iesl/canvas/sameer/dat/wiki-link/data-00000-of-00001");
    public static final String wiliFiles = conf.getProperty("wili.files","/iesl/canvas/martin/wiki-link/retrieve/pages");
    public static final String wiliFileIds = conf.getProperty("wili.fileids","/iesl/canvas/martin/wiki-link/retrieve/wiki-link-urls.dat");
    public static final int restrictWiliPages = Integer.parseInt(conf.getProperty("wili.restrictPages",Integer.MAX_VALUE+ ""));

    public static final String lbjConfigFile = conf.getProperty("lbj.config","<path to lbj config file>");
    public static final String lbjDataPath = conf.getProperty("lbj.datapath","<path to lbj data>");
    public static final String lbjLcPath =  conf.getProperty("lbj.lcPath","<path to bin - where *.lc data is>");

    public static final String nlpExtractPathStanford = createDir(conf.getProperty("nlpextract.pathstanford","./extract-new/"));
    public static final String nlpExtractOutputPathStanford = createDir(conf.getProperty("nlpextract.outputpathstanford",nlpExtractPathStanford));
    public static final String nlpExtractListStanford = conf.getProperty("nlpextract.liststanford","extractListStanford");
    public static final String nlpExtractScriptStanford = conf.getProperty("nlpextract.scriptstanford","extractNer.sh");
    public static final String nlpExtractExecStanford = conf.getProperty("nlpextract.execstanford","./lib/stanford-corenlp-2012-04-09");

    
    public static final String galagoTermCounts = conf.getProperty("galago.termcounts","termcounts.txt");
    public static final boolean pipelineCrossVal = Boolean.parseBoolean(conf.getProperty("pipeline.crossval", ""+false));

    public static final boolean printRankDebug = Boolean.parseBoolean(conf.getProperty("eval.printranking","true"));
    public static final boolean parallelProcessing = Boolean.parseBoolean(conf.getProperty("debug.parallelProcessing","false"));

    public static final String modelDir = conf.getProperty("pipeline.modelFile", detailedEvalOutput+".model");
    public static final boolean retrainModel = Boolean.parseBoolean(conf.getProperty("pipeline.retrainModel", "true"));

    public static final String pathToFinalLinkabilityFile = conf.getProperty("wikifier.linkabilityFile");
    public static final String absolutePathToWikifierConfigFile = conf.getProperty("wikifier.wikifierConfigFile");
    public static final String wikifierOutputFile = conf.getProperty("wikifier.outputFile");
    public static final String logFile = conf.getProperty("wikifier.logFile");

    // pseudo relevance tac source index
    public static final boolean useNerContextInQuery = Boolean.parseBoolean(conf.getProperty("usenerinquery", "false"));
    public static final boolean usePseudoRel = Boolean.parseBoolean(conf.getProperty("pseudo.useforfeatures", "false"));
    public static final String galagoPseudoJsonParameterFile = conf.getProperty("pseudo.galagoJsonParameterFile","./config/galago-tacsource");
    public static final String galagoPseudoPort = conf.getProperty("pseudo.galagoPort");
    public static final int maxCandidatesPseudo =  Integer.parseInt(conf.getProperty("pseudo.candidates.maxCandidates", "10"));
    public static final String pseudoQueryType =  conf.getProperty("pseudo.querytype", "seqdep");
    public static final String nerNeighborQuerySelectMethod =  conf.getProperty("nerneighborqueryselectmethod", "trivial");
    public static final String nerNeighborQueryMethod =  conf.getProperty("nerneighborquerymethod", "trivial");
    public static final int nerNeighborQueryK = Integer.parseInt(conf.getProperty("nerneighborquery_k", "10"));
    public static final boolean useSentencesInCandidateQuery = Boolean.parseBoolean(conf.getProperty("use_sentences_in_candidate_query", "true"));

    public static final boolean overwriteSCMs =   Boolean.parseBoolean(conf.getProperty("overwrite_scms", "false"));

    public static final String serialComentionPath=conf.getProperty("serialcomention.path","scm");
    public static boolean noFirstPassQuery = Boolean.parseBoolean(conf.getProperty("no_first_pass_query", "true"));

    public static boolean useKbaNlpReader = Boolean.parseBoolean(conf.getProperty("useKbaNlpReader", "true"));

    static String createDir(String extractDirName)  {
        File extractDir = new File(extractDirName);
        if(!extractDir.exists()){
            boolean success = extractDir.mkdirs();
            if(!success) {
                System.err.println("Could not create extraction directory "+ extractDir);
            }
            else {
                System.out.println("created new extraction directory "+extractDir);
            }
        }
        return extractDirName;
    }

    public static final String gitHash = readGitHash();
    static String readGitHash(){
        File gitversionFile = new File("githash.txt");
        String fileContent = "unknown git hash";
        if(gitversionFile.exists()){
            try {
                fileContent = (new BufferedReader(new FileReader(gitversionFile))).readLine();
            } catch (IOException e) {

            }
        }
        return fileContent;
    }
}

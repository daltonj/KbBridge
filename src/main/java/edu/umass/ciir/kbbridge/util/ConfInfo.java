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

    public static final String galagoKbSrv = conf.getProperty("galagoKbSrv");
    public static final String galagoKbPort = conf.getProperty("galagoKbPort");
    public static final String galagoKbJsonParameterFile = conf.getProperty("galagoKbJsonParameterFile","./config/galago-fullwiki");
    public static final int maxEntityCandidates = Integer.parseInt(conf.getProperty("candidates.maxEntityCandidates", "10"));

    public static final String galagoDefaultSrv = conf.getProperty("galagoDefaultSrv");
    public static final String galagoDefaultPort = conf.getProperty("galagoDefaultPort");
    public static final String galagoDefaultJsonParameterFile = conf.getProperty("galagoDefaultJsonParameterFile", "./config/galago-fullwiki");

    public static final String candidateQueryType =conf.getProperty("candidates.queryType","default");

    public static final int numTrainQueries =  Integer.parseInt(conf.getProperty("pipeline.numTrainQueries", "-1"));
    public static final int numTestQueries =  Integer.parseInt(conf.getProperty("pipeline.numTestQueries", "-1"));
    public static final boolean galagoUseLocalIndex = Boolean.parseBoolean(conf.getProperty("galagoUseLocalIndex", "false"));

    public static final boolean useOracleCandidateGeneration = Boolean.parseBoolean(conf.getProperty("useOracleCandidateGeneration", "false"));


    public static final String[] rankingFeatures = conf.getProperty("features.ranking","nus,llcsurf").split(",");
    public static final String[] nilClassifyFeatures = conf.getProperty("features.nil","nus,llcsurf").split(",");


    public static final String detailedEvalOutput = conf.getProperty("eval.detailedOutput","eval.txt");

    public static final String idmap = conf.getProperty("idmapping","/iesl/canvas/jdalton/tac/data/tac-wiki-mapping");



    public static final String nlpExtractPathStanford = createDir(conf.getProperty("nlpextract.pathstanford", "./extract-new/"));


    public static final String galagoTermCounts = conf.getProperty("galago.termcounts","termcounts.txt");
    public static final boolean pipelineCrossVal = Boolean.parseBoolean(conf.getProperty("pipeline.crossval", ""+false));


    // pseudo relevance tac source index
    public static final boolean useNerContextInQuery = Boolean.parseBoolean(conf.getProperty("usenerinquery", "false"));
    public static final String galagoPseudoJsonParameterFile = conf.getProperty("pseudo.galagoKbJsonParameterFile","./config/galago-tacsource");
    public static final String galagoPseudoPort = conf.getProperty("pseudo.galagoKbPort");
    public static final int maxCandidatesPseudo =  Integer.parseInt(conf.getProperty("pseudo.candidates.maxEntityCandidates", "10"));
    public static final String pseudoQueryType =  conf.getProperty("pseudo.querytype", "seqdep");
    public static final String nerNeighborQuerySelectMethod =  conf.getProperty("nerneighborqueryselectmethod", "trivial");
    public static final String nerNeighborQueryMethod =  conf.getProperty("nerneighborquerymethod", "trivial");
    public static final int nerNeighborQueryK = Integer.parseInt(conf.getProperty("nerneighborquery_k", "10"));
    public static final boolean useSentencesInCandidateQuery = Boolean.parseBoolean(conf.getProperty("use_sentences_in_candidate_query", "true"));


    public static final String serializedFeaturePath =conf.getProperty("serializedFeaturePath","scm");
    public static boolean noFirstPassQuery = Boolean.parseBoolean(conf.getProperty("no_first_pass_query", "true"));

    public static boolean useTacIdMap = Boolean.parseBoolean(conf.getProperty("useTacIdMap", "false"));

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

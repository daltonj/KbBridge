package edu.umass.ciir.kbbridge.util;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

/**
 * Reads properties from a file in src/main/resources that is called with the following patterns (in that order)
 *
 * tacco<user>@<hostname>.properties
 *
 * tacco-<hostname>.properties
 *
 * tacco.properties
 */
public final class ConfigLoader {
    static final String resourcePath = "src/main/resources/";
    static final String project = "kbbridge";
    static final String username = System.getProperty("user.name","user");
    static final String manualProps = System.getProperty("kbbridge.props","");

    private static List<File> possibleFilenames() {
        String hostname;
        try {
            hostname = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            hostname = "host";
        }
        // drop domain from hostname and lowercase
        int dotIdx = hostname.indexOf('.');
        if(dotIdx != -1) {
            hostname = hostname.substring(0, dotIdx).toLowerCase();
        }


        List<File> filenames = new ArrayList<File>();
        filenames.add(new File(resourcePath+project+".properties"));
        if(hostname.startsWith("compute")) {
            filenames.add(new File(resourcePath+project+"-blake.properties"));
        }
        filenames.add(new File(resourcePath+project+"-"+hostname+".properties"));
        if(hostname.startsWith("compute")) {
            filenames.add(new File( resourcePath+project+"-"+username+"@blake.properties"));
        }
        filenames.add(new File(resourcePath+project+"-"+username+"@"+hostname+".properties"));

        if(!manualProps.isEmpty()){
            filenames.add(new File(resourcePath+manualProps));
        }


        return(filenames);

    }

    private static void localReadProps(Properties properties, List<File> possibleFiles){
        Collections.reverse(possibleFiles);
        for (File propFile : possibleFiles) {
            System.out.println("try loading kbbridge loading properties: " + propFile);
            try {
                FileInputStream propStream = new FileInputStream(propFile);
                properties.load(propStream);
                System.out.println("...loaded from "+propFile);
                return;
            } catch (IOException e) {
                System.out.println("Unable to load file : " + propFile.getAbsolutePath() + " " + e.getMessage());
            }
        }
        
        throw new RuntimeException("Could not find property file for project "+project+". " +
                "Options (last preferred): "+  possibleFilenames()+
                "Local directory is "+(new File("").getAbsolutePath()));

    }
    public static Properties readProps() {
//        Properties properties = new Properties();
        Properties properties = System.getProperties();
        localReadProps(properties,possibleFilenames());
        return(properties);
    }

}





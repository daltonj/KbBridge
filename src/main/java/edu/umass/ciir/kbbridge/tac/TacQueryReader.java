package edu.umass.ciir.kbbridge.tac;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

/**
 *
 */
public class TacQueryReader {
    private final TacQueryProcessor callback;
    private final String queryFileName;

    public TacQueryReader(TacQueryProcessor callback, String queryFileName) {

        this.callback = callback;
        this.queryFileName = queryFileName;
    }

    public static void main(String[] args) throws IOException, SAXException {

//        TacQueryProcessorTestImpl tacQueryProcessorImpl = new TacQueryProcessorTestImpl();
//        TacQueryReader me = new TacQueryReader(tacQueryProcessorImpl, RedisInfo.el2010trainAnnoFile);
//        me.readQueryFile();
//        tacQueryProcessorImpl.dumpTitles();

    }

    public void foundQuery(TacQuery q) {

        //        System.out.println("q = " + q);
        callback.processQuery(q);

    }

    public void readQueryFile() throws IOException, SAXException {
        QuerySaxHandler handler = new QuerySaxHandler();
        XMLReader xr = TacSaxHelper.createXMLReader(handler);
        FileReader r = new FileReader(queryFileName);

        xr.parse(new InputSource(r));
    }

    private class QuerySaxHandler extends TacDefaultHandler {
        private boolean inQuery = true;
        private HashMap<String, Boolean> flags = new HashMap<String, Boolean>();
        private HashMap<String, String> data = new HashMap<String, String>();

        private void resetQueryData() {
            data = new HashMap<String, String>();
            flags = new HashMap<String, Boolean>();
        }

        @Override
        public void startTag(String tagName, Attributes atts) throws SAXException {
            if ("query".equals(tagName)) {
                inQuery = true;
                resetQueryData();
                data.put("id", atts.getValue("id"));
            }

            flags.put(tagName.toLowerCase(), true);
        }

        @Override
        public void endTag(String tagName) throws SAXException {

            if ("query".equals(tagName)) {
                inQuery = false;
                foundQuery(new TacQuery(data.get("id")
                        , data.get("name")
                        , data.get("docid")
                        , data.get("enttype")
                        , data.get("nodeid") //todo why is this null?
                        , data.get("entity")
                ));
            } else {
                data.put(tagName, getResultString());
                resultStringBuffer = new StringBuffer();
            }
            flags.put(tagName.toLowerCase(), false);
        }

        @Override
        public boolean shouldTakeChars() {
            return isFlag("name") || isFlag("docid") || isFlag("enttype") || isFlag("nodeid") || isFlag("entity");
        }

        private boolean isFlag(String tag) {
            return flags.containsKey(tag) && flags.get(tag);
        }
    }
}

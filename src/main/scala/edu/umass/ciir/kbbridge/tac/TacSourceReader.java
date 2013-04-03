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
public class TacSourceReader {
    private final TacSourceHandler queryHandler;


    public TacSourceReader(TacSourceHandler queryHandler){

        this.queryHandler = queryHandler;
    }


    private class TacSourceSaxHandler extends TacDefaultHandler {
        private boolean inPost = true;
        private HashMap<String, Boolean> flags = new HashMap<String, Boolean>();
        private HashMap<String, String> data = new HashMap<String, String>();
        private StringBuilder fullText = new StringBuilder();

        private void resetQueryData() {
            data = new HashMap<String, String>();
            flags = new HashMap<String, Boolean>();
        }
        
        @Override
        public void startTag(String tagName, Attributes atts) throws SAXException {
            if("text".equalsIgnoreCase(tagName)){
                inPost = true;
            }
            if("doctype".equalsIgnoreCase(tagName)){
                fullText.append(getResultString());
                resetResultString();

                data.put("doctype-source", atts.getValue("SOURCE"));
            }
            flags.put(tagName.toLowerCase(), true);
        }

        @Override
        public void endTag(String tagName) throws SAXException {

            if("text".equalsIgnoreCase(tagName)){
                inPost = false;

                fullText.append(getResultString());
                resetResultString();

                String[] doctypes = {data.get("doctype"), data.containsKey("doctype-source") ? data.get("doctype-source") : ""};
                queryHandler.foundDoc(new TacSourceDocument(data.get("docid").trim(), data.get("headline")+" "+fullText.toString(), doctypes));
            }else {
                data.put(tagName.toLowerCase(), getResultString());
                if(isFlag("text")) fullText.append(getResultString());
                resetResultString();
            }
            flags.put(tagName.toLowerCase(), false);
        }

        @Override
        public boolean shouldTakeChars() {
            return isFlag("headline") ||isFlag("docid") || isFlag("doctype")|| (isFlag("text") && !isFlag("poster") && !isFlag("postdate") ) ;
        }
        private boolean isFlag(String tag){
            return flags.containsKey(tag) && flags.get(tag);
        }
    }

    public void readSourceFile(String sourceFile) throws IOException, SAXException {
        TacSourceSaxHandler handler = new TacSourceSaxHandler();
        XMLReader xr = TacSaxHelper.createXMLReader(handler);
        FileReader r = new FileReader(sourceFile);

        xr.parse(new InputSource(r));


    }

    public static void main(String[] args) throws IOException, SAXException {
        TacSourceReader me = new TacSourceReader(new TacSourceHandler() {
            public void foundDoc(TacSourceDocument doc) throws ResultFoundException {
                System.out.println("doc.getDocid() = " + doc.getDocid());
                System.out.println("text = " + doc.getFulltext());
            }
        });
        me.readSourceFile("/usr/aubury/scratch2/jdalton/entity-linking/tac-source-sample/eng-WL-11-174596-12958290.sgm");

        System.out.println("done");
    }
}

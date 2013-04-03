package edu.umass.ciir.kbbridge.tac;

/**
 *
 */
public class TacSourceDocument {
    private final String fulltext;
    private final String docid;
    private final String[] doctypeInfo;

    public TacSourceDocument(String docid, String fulltext, String[] doctypeInfo){

        this.fulltext = fulltext;
        this.docid = docid;
        this.doctypeInfo = doctypeInfo;
    }

    public String getFulltext() {
        return fulltext;
    }

    public String getDocid() {
        return docid;
    }

    public String[] getDoctypeInfo() {
        return doctypeInfo;
    }
}

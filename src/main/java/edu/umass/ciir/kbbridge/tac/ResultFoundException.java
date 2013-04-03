package edu.umass.ciir.kbbridge.tac;

import org.xml.sax.SAXException;

/**
*
*/
public class ResultFoundException extends SAXException {
    private final String resultStr;

    public ResultFoundException(String result){

        this.resultStr = result;
    }

    public String getResultStr() {
        return resultStr;
    }
}

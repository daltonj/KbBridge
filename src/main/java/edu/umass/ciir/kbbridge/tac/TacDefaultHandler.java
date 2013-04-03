package edu.umass.ciir.kbbridge.tac;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.util.Arrays;


abstract public class TacDefaultHandler extends DefaultHandler
{
    protected StringBuffer resultStringBuffer = new StringBuffer();

    public TacDefaultHandler ()
    {
        super();
    }

    @Override
    public void startDocument() throws SAXException {
        super.startDocument();
        resultStringBuffer = new StringBuffer();
    }

    @Override
    final public void startElement (String uri, String name,
                  String qName, Attributes atts) throws SAXException
    {

        String elementName = "";
        if ("".equals (uri))
            elementName = qName;
        else
            elementName = uri + name;

        startTag(elementName, atts);
    }


    @Override
    final public void endElement (String uri, String name, String qName)  throws SAXException
    {
        String elementName = "";
        if ("".equals (uri))
            elementName = qName;
        else
            elementName = uri + name;

        endTag(elementName);
    }


    final public void characters (char ch[], int start, int length)
    {
        if(shouldTakeChars()) {
            resultStringBuffer.append(Arrays.copyOfRange(ch, start, start + length));
        }
    }

    public void resetResultString() {
        resultStringBuffer = new StringBuffer();
    }
    public String getResultString() {
        return resultStringBuffer.toString();
    }
    abstract public void startTag(String tagName, Attributes atts) throws SAXException;
    abstract public void endTag(String tagName) throws SAXException;
    abstract public boolean shouldTakeChars();

}
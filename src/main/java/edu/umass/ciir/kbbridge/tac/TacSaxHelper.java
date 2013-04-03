package edu.umass.ciir.kbbridge.tac;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 *
 */
public class TacSaxHelper {
    public static String parse(File file, XMLReader xr) throws IOException {
        FileReader r = new FileReader(file);
        String result = null;
        try{

            xr.parse(new InputSource(r));
        } catch (ResultFoundException e){

            result = e.getResultStr();
        } catch (SAXException e) {
            throw new RuntimeException(e);
        } finally {
            r.close();
        }

        return result;

    }

    public static XMLReader createXMLReader(TacDefaultHandler handler) throws SAXException {
        XMLReader xr = XMLReaderFactory.createXMLReader();
        xr.setContentHandler(handler);
        xr.setErrorHandler(handler);
        return xr;
    }
}

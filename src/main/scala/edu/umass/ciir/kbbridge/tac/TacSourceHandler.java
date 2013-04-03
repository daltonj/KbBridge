package edu.umass.ciir.kbbridge.tac;

/**
 *
 */
public interface TacSourceHandler {
      void foundDoc(TacSourceDocument doc) throws ResultFoundException;
}

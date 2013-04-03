package edu.umass.ciir.kbbridge.tac;

/**
 *
 */
public interface TacQueryHandler {
    void foundQuery(TacQuery q) throws ResultFoundException;
}

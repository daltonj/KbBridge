package edu.umass.ciir.kbbridge.tac;

/**
*
*/
public class TacQuery {
    private final String queryId;
    private final String queryName;
    private final String queryDocId;
    private final String queryEntType;
    private final String queryNodeId;
    private final String queryEntity;

    public TacQuery(String queryId, String queryName, String queryDocId, String queryEntType, String queryNodeId, String queryEntity){

        this.queryId = queryId;
        this.queryName = queryName;
        this.queryDocId = queryDocId;
        this.queryEntType = queryEntType;
        this.queryNodeId = queryNodeId;
        this.queryEntity = queryEntity;
    }
    public String toString(){
        return "Query(id="+ getQueryId() +", name="+ getQueryName() +", docId="+ getQueryDocId() +", enttype="+ getQueryEntType() +", nodeid="+ getQueryNodeId() +", entity="+ getQueryEntity() + ")";
    }

    public String getQueryId() {
        return queryId;
    }

    public String getQueryName() {
        return queryName;
    }

    public String getQueryDocId() {
        return queryDocId;
    }

    public String getQueryEntType() {
        return queryEntType;
    }

    public String getQueryNodeId() {
        return queryNodeId;
    }

    public String getQueryEntity() {
        return queryEntity;
    }
}

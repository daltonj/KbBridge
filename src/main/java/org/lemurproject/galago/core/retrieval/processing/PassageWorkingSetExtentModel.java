package org.lemurproject.galago.core.retrieval.processing;

// BSD License (http://lemurproject.org/galago-license)

import org.lemurproject.galago.core.index.Index;
import org.lemurproject.galago.core.retrieval.LocalRetrieval;
import org.lemurproject.galago.core.retrieval.ScoredDocument;
import org.lemurproject.galago.core.retrieval.ScoredPassage;
import org.lemurproject.galago.core.retrieval.iterator.MovableExtentIterator;
import org.lemurproject.galago.core.retrieval.iterator.MovableScoreIterator;
import org.lemurproject.galago.core.retrieval.query.Node;
import org.lemurproject.galago.core.retrieval.query.StructuredQuery;
import org.lemurproject.galago.core.util.ExtentArray;
import org.lemurproject.galago.tupleflow.Parameters;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.PriorityQueue;


/**
 * Performs passage-level scoring within extents. Scores each passage within the named extent.
 * Useful for restricting passage retrieval to sections of the document.
 *
 * In contrast, the #inside operator will yield passages where only a fraction is inside the given extent.
 *
 * Can also score seqential sets of extents. (but it assumes no break in
 * extents) -- any gaps will be scored.
 *
 *
 * to use: --processingModel=[org.lemurproject.galago.core.retrieval.processing.PassageWorkingSetExtentModel]
 *  --extentQuery=true --extent=name --working=[names/numbers] --passageQuery==true --passageSize=[50] --passageShift=[25]
 *
 * This implementation is written for Galago 3.4 and is inspired by
 * {@link org.lemurproject.galago.core.retrieval.processing.WorkingSetExtentModel}
 * and {@link org.lemurproject.galago.core.retrieval.processing.WorkingSetPassageModel}
 *
 * @author sjh, dietz
 *
 */
public class PassageWorkingSetExtentModel extends ProcessingModel {
    LocalRetrieval retrieval;
    Index index;

    public PassageWorkingSetExtentModel(LocalRetrieval lr) {
        this.retrieval = lr;
        this.index = lr.getIndex();
    }

    @Override
    public ScoredDocument[] execute(Node queryTree, Parameters queryParams) throws Exception {

        PassageScoringContext context = new PassageScoringContext();
        context.cachable = false;

        if(!queryParams.get("passageQuery", false)){
            throw new RuntimeException("MUST SET passageQuery=true ;; otherwise scores will always be document level.");
        }

        // There should be a whitelist to deal with
        List l = queryParams.getList("working");
        if (l == null) {
            throw new IllegalArgumentException("Parameters must contain a 'working' parameter specifying the working set");
        }

        Class containedType = l.get(0).getClass();
        List<Integer> whitelist;
        if (Integer.class.isAssignableFrom(containedType)) {
            whitelist = (List<Integer>) l;
        } else if (Long.class.isAssignableFrom(containedType)) {
            // Sadly this will not directly translate for now - maybe when/if we move
            // Galago to using longs instead of ints...
            whitelist = new ArrayList<Integer>();
            for (Long docid : (List<Long>) l) {
                whitelist.add(docid.intValue());
            }
        } else if (String.class.isAssignableFrom(containedType)) {
            whitelist = retrieval.getDocumentIds((List<String>) l);
        } else {
            throw new IllegalArgumentException(
                    String.format("Parameter 'working' must be a list of longs or a list of strings. Found type %s\n.",
                            containedType.toString()));
        }
        Collections.sort(whitelist);

        // Following operations are all just setup
        int requested = (int) queryParams.get("requested", 1000);
        // passageSize and shift can be used to cover a set of extents, instead of just one
        int passageSize = (int) queryParams.getLong("passageSize");
        int passageShift = (int) queryParams.getLong("passageShift");
        //int extentSetSize = (int) queryParams.get("extentCount", 1);
        //int extentShift = (int) queryParams.get("extentShift", 1);

        if (passageSize <= 0 || passageShift <= 0) {
            throw new IllegalArgumentException("passageSize/passageShift must be specified as positive integers.");
        }
//      if(extentSetSize <= 0 || extentShift <= 0){
//      throw new IllegalArgumentException("extentCount/extentShift must be specified as positive integers.");
//    }

        // scoring iterator
        MovableScoreIterator iterator =
                (MovableScoreIterator) retrieval.createIterator(queryParams,
                        queryTree,
                        context);

        // get the extent iterator
        String extent = queryParams.getString("extent");
        MovableExtentIterator extentIterator =
                (MovableExtentIterator) retrieval.createIterator(new Parameters(),
                        StructuredQuery.parse("#extents:" + extent + ":part=extents()"),
                        context);

        if (extentIterator.isDone()) {
            System.err.println("Failed to find iterator for extent " + extent);
            return null;
        }

        PriorityQueue<ScoredPassage> queue = new PriorityQueue<ScoredPassage>(requested);

        // now there should be an iterator at the root of this tree
        for (int i = 0; i < whitelist.size(); i++) {

            int document = whitelist.get(i);
            context.document = document;

            extentIterator.syncTo(document);

            ExtentArray extents = extentIterator.extents();
            if (extents.size() == 0) {
                // nothing to score, skip to next document
                continue;
            }

            // otherwise we have something to score, shift the scorer
            iterator.syncTo(document);

            // passageSize, passageShift defaults to 1: all extents are scored individually.

            for (int e = 0; e < extents.size(); e += 1) {
                // document len = extents.end(e);
                int length = extents.end(e);
                context.begin = extents.begin(e);
                context.end = Math.min(passageSize, length);

                boolean lastIteration = false;
                while (context.begin < length && !lastIteration) {
                    if (context.end >= length) {
                        lastIteration = true;
                    }

                    if (iterator.hasMatch(document)) {
                        double score = iterator.score();
                        if (requested < 0 || queue.size() <= requested || queue.peek().score < score) {
                            ScoredPassage scored = new ScoredPassage(document, score, context.begin, context.end);
//                      if (annotate) {
//                          scored.annotation = iterator.getAnnotatedNode();
//                      }
                            queue.add(scored);
                            if (requested > 0 && queue.size() > requested) {
                                queue.poll();
                            }
                        }
                    }

                    // Move the window forward
                    context.begin += passageShift;
                    // end must be bigger or equal to the begin, and less than the length of the document
                    context.end = Math.max(context.begin, Math.min(passageSize + context.begin, length));
                }
//        context.begin = extents.begin(e);
//
//        // if the window extends past the end of the array:
//        if ((e + extentSetSize - 1) >= extents.size()) {
//          context.end = extents.end(extents.size() - 1);
//        } else {
//          context.end = extents.end(e + extentSetSize - 1);
//        }
//
//        if (iterator.hasMatch(document)) {
//
//          double score = iterator.score();
//          if (requested < 0 || queue.size() <= requested || queue.peek().score < score) {
//            ScoredPassage scored = new ScoredPassage(document, score, context.begin, context.end);
//            queue.add(scored);
//            if (requested > 0 && queue.size() > requested) {
//              queue.poll();
//            }
//          }
//        }
//
//        // if we're done - break
//        if (context.end == extents.end(extents.size() - 1)) {
//          break;
//        }
            }
        }
        return toReversedArray(queue);
    }
}

package edu.umass.ciir.kbbridge.nil;

import com.aliasi.io.LogLevel;
import com.aliasi.io.Reporter;
import com.aliasi.io.Reporters;
import com.aliasi.matrix.DenseVector;
import com.aliasi.matrix.Vector;
import com.aliasi.stats.AnnealingSchedule;
import com.aliasi.stats.LogisticRegression;
import com.aliasi.stats.RegressionPrior;

import java.util.ArrayList;
import java.util.List;

public class LogisticRegressionNilClassifier {


    List<List<Double>> trainingVectors = new ArrayList<List<Double>>();
    List<Integer> trainingLabels = new ArrayList<Integer>();
    private LogisticRegression regression;

    public void train(List<List<Double>> trainingExampleVectors, List<Integer> labels) {

        Vector[] trainingExamples = new Vector[trainingExampleVectors.size()];

        for (int j = 0; j < trainingExampleVectors.size(); j++) {
            List<Double> vectorList = trainingExampleVectors.get(j);
            double[] vector = new double[vectorList.size()];
            for (int i = 0; i < vectorList.size(); i++) {
                vector[i] = vectorList.get(i);
            }
            DenseVector vect = new DenseVector(vector);
            trainingExamples[j] = vect;
        }

        int[] trainingLabels = new int[labels.size()];
        for (int l = 0; l < labels.size(); l++) {
            trainingLabels[l] = labels.get(l);
        }

        AnnealingSchedule annealingSchedule = AnnealingSchedule.exponential(0.00025, 0.999);
        RegressionPrior prior = RegressionPrior.gaussian(1.0d, true);


        Reporter reporter = Reporters.stdOut().setLevel(LogLevel.INFO);

        regression = LogisticRegression.estimate(trainingExamples,
                trainingLabels,
                prior,
                annealingSchedule,
                reporter, // reporter with no feedback
                0.000000001, // min improve
                100, // min epochs
                10000); // max epochs

        // print weights
        Vector[] betas = regression.weightVectors();
        for (int outcome = 0; outcome < betas.length; ++outcome) {
            System.out.print("Outcome=" + outcome);
            for (int i = 0; i < betas[outcome].numDimensions(); ++i)
                System.out.printf(" %6.2f", betas[outcome].value(i));
            System.out.println();
        }


    }

    public List<Double> predict(List<Double> features, String query) {

        double[] vector = new double[features.size()];
        for (int i = 0; i < features.size(); i++) {
            vector[i] = features.get(i);
        }
        DenseVector vect = new DenseVector(vector);

        double[] conditionalProbs = regression.classify(vect);

//        for (int i = 0; i < vect.numDimensions(); ++i) {
//            System.out.printf("%3.2f ",vect.value(i));
//        }

        List<Double> conditionals = new ArrayList<Double>();
        for (int k = 0; k < conditionalProbs.length; ++k) {
            //System.out.printf(" p(%d|input)=%4.3f ",k,conditionalProbs[k]);
            conditionals.add(conditionalProbs[k]);
        }
        //System.out.println("\n");

        return conditionals;
    }
}

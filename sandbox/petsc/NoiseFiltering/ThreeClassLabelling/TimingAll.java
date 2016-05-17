/* Author : Kanika Sood
 // does classification with rf and gives the list of good and bad solvers for a given linear system
  * Input : An arff file with 
 */

package ThreeClassLabelling;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import weka.classifiers.meta.FilteredClassifier;

import weka.classifiers.Evaluation;
import weka.classifiers.bayes.BayesNet;
import weka.classifiers.trees.J48;
import weka.classifiers.trees.RandomForest;
import weka.classifiers.*;
import weka.core.Attribute;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ArffLoader.ArffReader;
import weka.filters.unsupervised.attribute.RemoveUseless;

public class TimingAll {
	
	public static void main(String[] args) throws Exception {
		BufferedReader datafile = readDataFile("/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/solvers_anamod_25_40_RS2.arff");		
		 
		// Load instances from .arff file 
		Instances DataSet = new Instances(datafile);
		int size= 0;
		size = DataSet.numInstances();
		System.out.println("Number of instances: "+ size);
		int trainSize = (int) Math.round(DataSet.numInstances() * 0.66);
		int testSize = DataSet.numInstances() - trainSize;
		DataSet.setClassIndex(DataSet.numAttributes() - 1); 
		System.out.println("Number of instances: "+ size+ "  "+ trainSize + "  "+  testSize);
		Instances train = new Instances(DataSet, 0, trainSize);
		Instances test = new Instances(DataSet, trainSize, testSize);
		
		// Set class index to the last attribute
		 DataSet.setClassIndex(DataSet.numAttributes() - 1); 
		 RemoveUseless filter = new RemoveUseless();
		 FilteredClassifier fc = new FilteredClassifier();
		 fc.setFilter(filter);		 

		// Build Classifier 
		 RandomForest rf = new RandomForest();
		 rf.setNumTrees(100);
		 rf.setNumTrees(100);
		 rf.setMaxDepth(0);
		 rf.setNumFeatures(0);
		 rf.setSeed(1);
		 fc.setClassifier(rf);
		try {
			rf.buildClassifier(train);
		} catch (Exception e) {
			e.printStackTrace();
		} 
		// Test the model 
		Evaluation validation;
		try {
			//validation = new Evaluation(test);
			Evaluation evaluation = new Evaluation(train);
			//training 
			//testing
			evaluation.evaluateModel(rf, test);
			validation= evaluation;
			//validation.crossValidateModel(rf, DataSet, 10, new Random(1)); 
			String summary = validation.toSummaryString(); 
		
		int count=0; 
		int cnt = 0;
		int correct= 0;
		String clabel="";
		int mispredictions= 0;
		BufferedWriter writer = new BufferedWriter(new FileWriter("/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RS2mispredictedAPlabels.csv"));
		writer.write("avg-diag-dist" + "," + "norm1" + "," + "col-variability" + "," + "min-nnzeros-per-row" + "," + "row-variability" + "," + "kappa" + "," + "TIME" + "," + "solver" + "," + "class" + "," + "predicted-class" );
		writer.newLine();
		for(int j=0; j < DataSet.numInstances();j++){  
		            double prediction = rf.classifyInstance(DataSet.instance(j)); //predicts as 0 or 1; 0 for good solvers 
		            double actual = DataSet.instance(j).classValue(); 
		            if(prediction == 0.0)
	            		clabel = "good";
	            	else if(prediction == 1.0)
            		clabel = "fair";
	            	else if(prediction == 2.0)
	            		clabel = "bad";
		            if(prediction == 0.0 && actual == 1.0 || prediction == 0.0 && actual == 2.0 || prediction == 1.0 && actual == 0.0 || prediction == 1.0 && actual == 2.0 || prediction == 2.0 && actual == 1.0 || prediction == 2.0 && actual == 0.0){
		            	mispredictions++;
		            	System.out.println(DataSet.instance(j).toString() + "," + clabel);
		            writer.write(DataSet.instance(j).toString() + "," + clabel); // it writes the actual label in the writer file
		            writer.newLine();
		            }
		     
		            }
		
		System.out.println(rf.toString());
		System.out.println("=== Summary ===");
		System.out.println(validation.toSummaryString());
		System.out.println(validation.toClassDetailsString());
		//System.out.println("Check this : " + validation.pctCorrect());
		//System.out.println("TPR -->" + validation.truePositiveRate(1)); //0 for good class, 1 for bad class
		double accuracy = 0;
		accuracy = validation.confusionMatrix()[0][0]/(validation.confusionMatrix()[0][0] + validation.confusionMatrix()[0][1]);
		
		System.out.println("accuracy : "+accuracy* 100.0);
		System.out.println("=== My confusion matrix === " );
		System.out.println("a"+ "\t" +"b" + "\t"  +"c" + "\t" + "<-- classified as ");
		System.out.println(validation.confusionMatrix()[0][0] + "\t" +validation.confusionMatrix()[0][1] + "\t" +validation.confusionMatrix()[0][2] + "\t" + "|" + "a = good");
		System.out.println(validation.confusionMatrix()[1][0] + "\t" +validation.confusionMatrix()[1][1] + "\t" +validation.confusionMatrix()[1][2] + "\t" + "|" + "b = fair");
		System.out.println(validation.confusionMatrix()[2][0] + "\t" +validation.confusionMatrix()[2][1] + "\t" +validation.confusionMatrix()[2][2] + "\t" + "|" + "b = bad");
	
		
		
		int badCount = 0;
		List goodSolvers = new ArrayList();
		List badSolvers = new ArrayList();
		Instances testData;

		} catch (Exception e) {
			System.out.println("Error"+e);
			e.printStackTrace();
		} 
		
		}

	private static BufferedReader readDataFile(String filename) {
		BufferedReader inputReader = null;
		// read the arff data file
		try {
			inputReader = new BufferedReader(new FileReader(filename));
		} catch (FileNotFoundException ex) {
			System.err.println("File not found: " + filename);
		}
		return inputReader;
	}
	
	

}

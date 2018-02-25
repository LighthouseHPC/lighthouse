/* Author : Kanika Sood
 // does classification with RF and gives the list of good and bad solvers for a given linear system
  * Input : An arff file with 
 */

package ThreeClassLabelling;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import weka.classifiers.meta.FilteredClassifier;

import weka.classifiers.Evaluation;
import weka.classifiers.trees.RandomForest;
import weka.classifiers.*;
import weka.core.Attribute;
import weka.core.Instance;
import weka.core.Instances;
import weka.filters.unsupervised.attribute.RemoveUseless;

public class MisPredictionsRFTTSplitRS1 {
	
	public static void main(String[] args) throws Exception {
		//BufferedReader datafile = readDataFile("/Users/kanikas/Desktop/petsc_anamod_35.arff");
		//BufferedReader datafile = readDataFile("/Users/kanikas/Documents/research/solvers_anamod_35_mname_stime.arff"); //working one
		BufferedReader datafile = readDataFile("/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/solvers_anamod_25_40_RS1.arff");
		//BufferedReader datafile = readDataFile("/Users/kanikas/Documents/research/petsc_anamodRS1_35.arff");
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
		 //RemoveUseless filter = new RemoveUseless();
		 //FilteredClassifier fc = new FilteredClassifier();
		 //fc.setFilter(filter);		 

		// Build Classifier 
		 RandomForest rf = new RandomForest();
		 rf.setNumTrees(100);
		 rf.setMaxDepth(0);
		 rf.setNumFeatures(0);
		 rf.setSeed(1);
		// fc.setClassifier(rf);
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
		int mispredictions= 0;
		BufferedWriter writer = new BufferedWriter(new FileWriter("/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS1/RFmispredicted_3labels21505_RS1.csv"));
		for(int j=0; j < DataSet.numInstances();j++){  
		            double prediction = rf.classifyInstance(DataSet.instance(j)); //predicts as 0 or 1; 0 for good solvers 
		            double actual = DataSet.instance(j).classValue(); 
		            if(prediction == 0.0 && actual == 1.0 || prediction == 0.0 && actual == 2.0 || prediction == 1.0 && actual == 0.0 || prediction == 1.0 && actual == 2.0 || prediction == 2.0 && actual == 1.0 || prediction == 2.0 && actual == 0.0){
		            	mispredictions++;
		            	//DataSet.instance(j).
		            	writer.write(DataSet.instance(j).toString());
		            	writer.newLine();
		            	//System.out.println("Predicted as good, but actually were bad:" +DataSet.instance(j));
		         }
		            else
		            	correct++;
		            //writer.close();
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
		System.out.println("a"+ "\t" +"b" + "\t" +"<-- classified as ");
		System.out.println(validation.confusionMatrix()[0][0] + "\t" +validation.confusionMatrix()[0][1] + "\t" +validation.confusionMatrix()[0][2] + "\t" + "|" + "a = good");
		System.out.println(validation.confusionMatrix()[1][0] + "\t" +validation.confusionMatrix()[1][1] + "\t" +validation.confusionMatrix()[1][2] + "\t" + "|" + "b = fair");
		System.out.println(validation.confusionMatrix()[2][0] + "\t" +validation.confusionMatrix()[2][1] + "\t" +validation.confusionMatrix()[2][2] + "\t" + "|" + "b = bad");
		System.out.println("Correct: " + correct + "  Mispredictions: " + mispredictions);
		
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
	
	//read test instance file 
	public static Instances TestInstance() {
		int[] solverList = new int[154]; //read from the arff file 
	
		BufferedReader testFile = readDataFile("/Users/kanikas/Desktop/PETSC_ReducedSets/TestInstance.arff");
		// Load instances from .arff file 
		Instances testData = null;
		try {
			testData = new Instances(testFile);
			System.out.println("Reading test instance file");
			//get no. of attributes in the arff file
			int attributes = 0;
			attributes = testData.numAttributes();
			Instance instance = testData.lastInstance();
			System.out.println("No. of attributes in test instance : " + attributes);
			int noOfInstances = solverList.length; //get how many solvers are there
			//now add the last column as solver in the testInstance
			//adding attribute name
			//Attribute solverId = new Attribute("solverId");
			//testData.insertAttributeAt(solverId,8);
			//Attribute category = new Attribute("class");
			//testData.insertAttributeAt(category,9);	
			//copy the test instance that many times
			//no. of solvers = 153
			 for(int i = 0; i< 153;i++){	 
				testData.add(instance);
				testData.instance(i).setValue(8, i);
			}
			 //testData.delete(0);
			System.out.println("Instance -->"+ instance);
		
		} catch (IOException e) {
			System.out.println("Error"+e);
			e.printStackTrace();
		}
		return testData;	
	}

}

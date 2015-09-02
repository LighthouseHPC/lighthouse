/* Author : Kanika Sood
 // Replicates Weka GUI's classification with BayesNet and writes the output(Accuracy and confusion matrix) to a file
 //Uses Weka jar files
 */

package BayesNet;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Random;

import weka.classifiers.Evaluation;
import weka.classifiers.bayes.BayesNet;
import weka.classifiers.trees.J48;
import weka.classifiers.*;
import weka.core.Instances;

public class WekaReplicatedBN {

	public static void main(String[] args) throws Exception {
		BufferedReader datafile = readDataFile("../PETSc_ReducedSets/petsc_anamodRS1_35.arff");
		// Load instances from .arff file 
		Instances DataSet = new Instances(datafile);
		// Set class index to the last attribute
		 DataSet.setClassIndex(DataSet.numAttributes() - 1); 
		// Build Classifier 
		 BayesNet bn = new BayesNet(); 
		try {
			bn.buildClassifier(DataSet);
		} catch (Exception e) {
			e.printStackTrace();
		} 
		// Test the model 
		Evaluation validation;
		try {
			validation = new Evaluation(DataSet);
		
			validation.crossValidateModel(bn, DataSet, 10, new Random(1)); 
		String summary = validation.toSummaryString(); 
		System.out.println(summary); 
		
		
		int count=0; 
		for(int j=0; j < DataSet.numInstances();j++){  
					//System.out.println("Instance"+DataSet.instance(j));
		            double prediction = bn.classifyInstance(DataSet.instance(j)); 
		            double actual = DataSet.instance(j).classValue();
		            
		            System.out.println(prediction+"--" +actual);
		          //  if (pred == actual){ 
		            //   count++; 
		             //} 
		         } 
 
		
		System.out.println(bn.toString());
		System.out.println("=== Summary ===");
		System.out.println(validation.toSummaryString());
		System.out.println(validation.toClassDetailsString());
		//System.out.println("Check this : " + validation.pctCorrect());
		//System.out.println("TPR -->" + validation.truePositiveRate(1)); //0 for good class, 1 for bad class
		System.out.println("accuracy : "+validation.confusionMatrix()[0][0]/(validation.confusionMatrix()[0][0] + validation.confusionMatrix()[0][1]));
		//System.out.println(validation.confusionMatrix()[0][0] + "\t" + validation.confusionMatrix()[0][1] + "\t" + validation.confusionMatrix()[1][0] + "\t" 
		//		+ validation.confusionMatrix()[1][1] + "\t" );
		System.out.println("=== My confusion matrix === " );
		System.out.println("a"+ "\t" +"b" + "\t" +"<-- classified as ");
		System.out.println(validation.confusionMatrix()[0][0] + "\t" +validation.confusionMatrix()[0][1] + "\t" + "|" + "a = good");
		System.out.println(validation.confusionMatrix()[1][0] + "\t" +validation.confusionMatrix()[1][1] + "\t" + "|" + "b = bad");
		

		//FileOutputStream outputFile = new FileOutputStream("/Users/kanikas/Desktop/PETSc_ReducedSets/Output/BayesNetClassificationResult.txt");
		PrintWriter outputFile = new PrintWriter("../Output/BayesNetClassificationResult.txt", "UTF-8");
		outputFile.println("This file is wriiten by WekaReplicatedBN.java program");
		outputFile.println(validation.toSummaryString());
		outputFile.println(validation.toClassDetailsString());
		outputFile.println("accuracy : "+validation.confusionMatrix()[0][0]/(validation.confusionMatrix()[0][0] + validation.confusionMatrix()[0][1]));
		outputFile.println("=== Confusion matrix ===");
		outputFile.println("a"+ "\t" +"b" + "\t" +"<-- classified as ");
		outputFile.println(validation.confusionMatrix()[0][0] + "\t" +validation.confusionMatrix()[0][1] + "\t" + "|" + "a = good");
		outputFile.println(validation.confusionMatrix()[1][0] + "\t" +validation.confusionMatrix()[1][1] + "\t" + "|" + "b = bad");
		outputFile.close();
		
		
		} catch (Exception e) {
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

/* Author : Kanika Sood
 does classification with BayesNet and gives the list of good and bad solvers for a given linear system
   Input : An arff file for training(Reduced Set 1 for PETSc with b = 35 ) and an arff for testing with only one test instance i.e. one linear system
  This file requires weka-src.jar and weka.jar
 */


import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import weka.classifiers.Evaluation;
import weka.classifiers.bayes.BayesNet;
import weka.classifiers.trees.J48;
import weka.classifiers.*;
import weka.core.Attribute;
import weka.core.Instance;
import weka.core.Instances;


public class GoodSolvers {
	
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
		            double prediction = bn.classifyInstance(DataSet.instance(j)); 
		            double actual = DataSet.instance(j).classValue(); 
		         }
		int badCount = 0;
		List goodSolvers = new ArrayList();
		List badSolvers = new ArrayList();
		Instances testData;
		testData = TestInstance();
		System.out.println(testData);
		System.out.println("length -->" + testData.numInstances());
		testData.setClassIndex(9);
		//classifying for test data
		for(int j=0; j < testData.numInstances();j++){  	
			System.out.println(testData.instance(j));
            double prediction = bn.classifyInstance(testData.instance(j)); 
			//double[] prediction = bn.distributionForInstance(testData.instance(j)); 
            double actual = testData.instance(j).classValue(); 
            System.out.println(prediction + "<=========>" + actual);
            if (prediction == 0.0){  // for good solvers
               count++;
               goodSolvers.add(testData.instance(j).stringValue(8));  
               
             } 
            else //for bad solvers 
            {
            	badCount++;
            	badSolvers.add(testData.instance(j).stringValue(8));
            }
         } 
			
		System.out.println("No. of Good solvers found -->" + count + "\t GoodSolver List ->"+ goodSolvers);
		System.out.println("No. of Bad solvers found -->" + badCount + "\t Bad Solver List ->" +  badSolvers);
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
	
		BufferedReader testFile = readDataFile("../PETSc_ReducedSets/TestInstance.arff");
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


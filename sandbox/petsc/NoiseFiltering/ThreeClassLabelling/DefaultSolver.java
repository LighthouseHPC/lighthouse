package ThreeClassLabelling;

import BayesNet.CsvReader;

	
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;
import java.io.*;
import java.lang.*;

public class DefaultSolver {
		List<RowObject> fileOne = new ArrayList<>();
		List<RowObject> fileTwo = new ArrayList<>();
		HashMap default_time = new HashMap<>();

		public static void main(String[] args) throws IOException {
			new CsvComparison().read();
			;
		}

		void read() {
	
			fileTwo = read("/Users/kanikas/Desktop/mispredicted.csv", false);
			System.out.println("File size======================================"
					+ fileTwo.size());
			write();

		}

		List<RowObject> read(String filename, boolean lastTwo) {
			try {
				// use FileWriter constructor that specifies open for appending

				List<RowObject> data = new ArrayList<>();
				CsvReader features = new CsvReader(filename);
				features.readHeaders();
				
				while (features.readRecord()) {
					RowObject row = new RowObject();
					for (int i = 0; i < 71; i++) {
						row.add(features.get(i));

					}
					row.id(features.get(68));
					System.out.println(features.get(68));
					if(features.get(68).equals("32168839")){
						default_time.put(features.get(68), features.get(70));
					}
							
					if (lastTwo) {
						row.add(features.get("matrix_name"),
								features.get("solver_time"));
					}

					data.add(row);
					//System.out.println("" + data.size());
				}

				features.close();
				return data;

			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
			return null;
		}

		void write() {

			try {
				// use FileWriter constructor that specifies open for appending
				String file = "/Users/kanikas/Desktop/defaultTime.csv";
				CsvWriter csvOutput = new CsvWriter(new FileWriter(file, false),
						',');

				boolean alreadyExists = new File(file).exists();
				// if the file didn't already exist then we need to write out the
				// header line

				CsvReader features = new CsvReader(
						"/Users/kanikas/Desktop/mispredicted.csv");
				features.readHeaders();

				for (int i = 0; i < 69; i++) {
					csvOutput.write(features.getHeader(i));
					//System.out.println("" + features.getHeader(i));
				}
				csvOutput.write("matrix_name");
				csvOutput.write("solver_time");
				csvOutput.endRecord();

				// else assume that the file already has the correct header line

				for (RowObject rowFileOne : fileOne) {
					for (RowObject rowFileTwo : fileTwo) {
						if (rowFileOne.equals(rowFileTwo)) {
							
							for (String value : rowFileOne.colValues) {
								csvOutput.write(value.toString());
							}
							csvOutput.write(rowFileOne.id);
							csvOutput.write(rowFileOne.matrix_name);
							csvOutput.write(rowFileOne.solver_time);
							csvOutput.endRecord();
							break;
						}
					}
				}

				csvOutput.close();
			} catch (IOException e) {
				e.printStackTrace();
			}

		}
	

}

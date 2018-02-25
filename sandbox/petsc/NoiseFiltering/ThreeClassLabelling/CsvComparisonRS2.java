package ThreeClassLabelling;

/*
 * Input has class label as -1 and 1 so changing it to good and bad in this file.
 */

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.io.*;
import java.lang.*;

public class CsvComparisonRS2 {

	List<RowObject> fileOne = new ArrayList<>();
	List<RowObject> fileTwo = new ArrayList<>();

	public static void main(String[] args) throws IOException {
		new CsvComparisonRS2().read();
	}

	void read() {
		fileOne = read(
				"/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/MatrixNameWithFeatVal3class_40RS2.csv",
				true, false);
		System.out.println("File size-------------------------------------"
				+ fileOne.size());

		fileTwo = read(
				"/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RS2mispredictedAPlabelsNoNoise1.csv",
				false, true);
		
//		fileTwo = read(
//				"/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RS2mispredicted443.csv",
//				false, true);

		System.out.println("File size======================================"
				+ fileTwo.size());
		write();
	}

	List<RowObject> read(String filename, boolean lastTwo, boolean labels) {
		try {
			// use FileWriter constructor that specifies open for appending

			List<RowObject> data = new ArrayList<>();
			CsvReader features = new CsvReader(filename);
			features.readHeaders();

			while (features.readRecord()) {
				RowObject row = new RowObject();
				for (int i = 0; i <= 5; i++) {
					row.add(features.get(i));
					// System.out.println(features.get(i));

				}
				row.id(features.get(7));
				
				// System.out.println(features.get("kappa")+"hi");
				// System.out.println(features.get(71));

				if (lastTwo) {
					row.classlabel(features.get(10));
					// System.out.println(features.get("matrix_name"));
					row.add(features.get("matrix_name"),
							features.get("solver_time"));
				}

				if (labels) {
					row.predicted_class = features.get("predicted-class");
				}

				data.add(row);
				// System.out.println("" + data.size());
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
			// String file =
			// "/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/combined21505_feb22.csv";
			//String file = "/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RFRS2combined.csv";

			String file = "/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RFRS2combinedNoNoise.csv";
			CsvWriter csvOutput = new CsvWriter(new FileWriter(file, false),
					',');

			boolean alreadyExists = new File(file).exists();
			// if the file didn't already exist then we need to write out the
			// header line

			// CsvReader features = new
			// CsvReader("/Users/kanikas/Documents/research/solvers_anamod_35_mname_stime.csv");
			CsvReader features = new CsvReader(
					"/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/MatrixNameWithFeatVal3class_40RS2.csv");
			features.readHeaders();

			for (int i = 0; i <= 5; i++) {
				csvOutput.write(features.getHeader(i));
				// System.out.println("" + features.getHeader(i));
			}
			csvOutput.write("solver");
			csvOutput.write("class");
			csvOutput.write("matrix_name");
			csvOutput.write("solver_time");
			csvOutput.write("predicted-class");
			csvOutput.endRecord();

			// else assume that the file already has the correct header line
			int ctr = 0;
			for (RowObject rowFileTwo : fileTwo) {
				for (RowObject rowFileOne : fileOne) {
					if (rowFileOne.equals(rowFileTwo)) {
						// System.out.println("Count" + ctr++);
						for (String value : rowFileOne.colValues) {
							csvOutput.write(value);
							// System.out.println(rowFileOne.colValues);
						}
						csvOutput.write(rowFileOne.id);
						if (rowFileOne.classlabel.equals("-1"))
							rowFileOne.classlabel = "bad";
						else if (rowFileOne.classlabel.equals("1"))
							rowFileOne.classlabel = "good";
						
						csvOutput.write(rowFileOne.classlabel);
						
						System.out.println(rowFileOne.colValues + ","
								+ rowFileOne.id + "," + rowFileTwo.classlabel
								+ rowFileOne.matrix_name + ","
								+ rowFileOne.solver_time);
						
						csvOutput.write(rowFileOne.matrix_name);
						csvOutput.write(rowFileOne.solver_time);
						csvOutput.write(rowFileTwo.predicted_class);
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

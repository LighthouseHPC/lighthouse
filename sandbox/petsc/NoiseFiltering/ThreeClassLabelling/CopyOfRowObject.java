package ThreeClassLabelling;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

public class CopyOfRowObject {
	List<String> colValues = new ArrayList<>();
	String id;
	String classlabel;
	String matrix_name;
	String solver_time;

	void add(String f) {
		
		Double value = Double.parseDouble(f);
		/*
		 * DecimalFormat df = new DecimalFormat("0.00");
		 * df.setMaximumFractionDigits(2); value =
		 * Double.parseDouble(df.format(value));
		 */
		Long val = value.longValue();
		colValues.add(f);

	}

	void id(String id) {
		this.id = id;
	}
	
	void classlabel(String classlabel) {
		this.classlabel = classlabel;
	}

	void add(String s1, String s2) {
		matrix_name = s1;
		solver_time = s2;
	}

	public boolean equals(CopyOfRowObject obj) {
		// TODO Auto-generated method stub
		return colValues.equals(obj.colValues) && this.id.equals(obj.id);
	}
}

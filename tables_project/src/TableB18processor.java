import java.io.*;
import java.util.*;
import java.sql.*;
import descriptorAnalyzer.*;

import TableB18.jabx.*;

public class TableB18processor {
	
	protected List<BUFRCREX1800TableBEn> tbentries;

	//  Constructor methods.
	
	public TableB18processor() {
		tbentries.clear();
	}
	
	public TableB18processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getBUFRCREX1800TableBEn();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Table B entries.
	
	public void writeList(FileWriter fw) {
		String fxy;
		String outline;
		DescriptorAnalyzer da = new DescriptorAnalyzer();
		
		if ( tbentries.size() > 0 ) {
			try {
				for (BUFRCREX1800TableBEn tbentry : tbentries) {
			    	fxy = tbentry.getFXY();
			    	outline = String.format("Entry #%5d %s %60s %s %s-%s-%s %15s %3s %12s %4s %16s  >%s<\n",
			    			tbentry.getNo().intValue(), tbentry.getClassNo(), tbentry.getClassNameEn(), fxy,
			    			da.getF(fxy), da.getX(fxy), da.getY(fxy),
			    			tbentry.getStatus(), tbentry.getBUFRScale(), tbentry.getBUFRReferenceValue(),
			    			tbentry.getBUFRDataWidthBits(), tbentry.getBUFRUnit(), tbentry.getElementNameEn());
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no TableB entries in the list!");
		}
		
	}
	
	//  Method to ingest all Table B entries into the database.
	
	public void ingest(FileWriter fw, Integer ven, Connection cn) {
		String fxy;
		String sqlstg;
		DescriptorAnalyzer da = new DescriptorAnalyzer();
		
		if ( tbentries.size() > 0 ) {
			try {
				sqlstg = "INSERT INTO bufr_b" +
				         "(f, x, y, version_idx, element_name, class," +
				         " bufr_unit, bufr_scale, bufr_reference_value," +
				         " bufr_data_width, fxy) " +
				         "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
				PreparedStatement updateDB = cn.prepareStatement(sqlstg);
				for (BUFRCREX1800TableBEn tbentry : tbentries) {
					// Insert this entry into the bufr_b table.
					if ( ! tbentry.getStatus().contains("alidatio")) {  // Unless it has "Validation" status.
						fxy = tbentry.getFXY();
						updateDB.setInt(1, Integer.parseInt(da.getF(fxy)));
						updateDB.setInt(2, Integer.parseInt(da.getX(fxy)));
						updateDB.setInt(3, Integer.parseInt(da.getY(fxy)));
						updateDB.setInt(4, ven);
						updateDB.setString(5, tbentry.getElementNameEn());
						updateDB.setString(6, tbentry.getClassNameEn());
						updateDB.setString(7, tbentry.getBUFRUnit());
						updateDB.setInt(8, Integer.parseInt(tbentry.getBUFRScale()));
						updateDB.setInt(9, Integer.parseInt(tbentry.getBUFRReferenceValue()));
						updateDB.setInt(10, Integer.parseInt(tbentry.getBUFRDataWidthBits()));
						updateDB.setString(11, fxy);
						fw.write(updateDB.toString() + "\n");  // Write copy of SQL command to output file.
						updateDB.executeUpdate();
					}	
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no TableB entries in the list!");
		}
		
	}

}

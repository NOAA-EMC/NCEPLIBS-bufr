import java.io.*;
import java.util.*;
import java.sql.*;

import TableB14.jabx.*;

public class TableB14processor {
	
	protected List<ExportingBCTableBE> tbentries;

	//  Constructor methods.
	
	public TableB14processor() {
		tbentries.clear();
	}
	
	public TableB14processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingBCTableBE();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Table B entries.
	
	public void writeList(FileWriter fw) {
		String fxy;
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingBCTableBE tbentry : tbentries) {
			    	fxy = tbentry.getFXY();
			    	outline = String.format("Entry #%5d %s %60s %s %s-%s-%s %15s %3s %12s %4s %16s  >%s<\n",
			    			tbentry.getNo(), tbentry.getClassNo(), tbentry.getClassName(), fxy,
			    			fxy.substring(0,1), fxy.substring(1,3), fxy.substring(3,6),
			    			tbentry.getStatus(), tbentry.getBUFRScale(), tbentry.getBUFRReferenceValue(),
			    			tbentry.getBUFRDataWidthBits(), tbentry.getBUFRUnit(), tbentry.getElementName());
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
		
		if ( tbentries.size() > 0 ) {
			try {
				sqlstg = "INSERT INTO bufr_b" +
				         "(f, x, y, version_idx, element_name, class," +
				         " bufr_unit, bufr_scale, bufr_reference_value," +
				         " bufr_data_width, fxy) " +
				         "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
				PreparedStatement updateDB = cn.prepareStatement(sqlstg);
				for (ExportingBCTableBE tbentry : tbentries) {
					// Insert this entry into the bufr_b table.
					if ( ! tbentry.getStatus().contains("alidatio")) {  // Unless it has "Validation" status.
						fxy = tbentry.getFXY();
						updateDB.setInt(1, Integer.parseInt(fxy.substring(0,1)));
						updateDB.setInt(2, Integer.parseInt(fxy.substring(1,3)));
						updateDB.setInt(3, Integer.parseInt(fxy.substring(3,6)));
						updateDB.setInt(4, ven);
						updateDB.setString(5, tbentry.getElementName());
						updateDB.setString(6, tbentry.getClassName());
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

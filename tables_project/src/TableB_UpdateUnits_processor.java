import java.io.*;
import java.util.*;
import java.sql.*;

import TableB_UpdateUnits.jabx.*;

public class TableB_UpdateUnits_processor {
	
	protected List<ExportingBCTableBUpdateUnits> tbentries;

	//  Constructor methods.
	
	public TableB_UpdateUnits_processor() {
		tbentries.clear();
	}
	
	public TableB_UpdateUnits_processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingBCTableBUpdateUnits();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Table B UpdateUnits entries.
	
	public void writeList(FileWriter fw) {
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingBCTableBUpdateUnits tbentry : tbentries) {
			    	outline = String.format("Entry:  Old Units >%s<    New Units >%s<\n",
			    			tbentry.getBUFRUnitOld(), tbentry.getBUFRUnitNew() );
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no TableB UpdateUnits entries in the list!");
		}
		
	}
	
	//  Method to update all Table B entries in the database by replacing BUFRUnitOld with BUFRUnitNew.
	
	public void update(FileWriter fw, Integer ven, Connection cn) {
		PreparedStatement getEN, updateDB;
		int[] idx_ent = new int[5000];
		int nidx_ent;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingBCTableBUpdateUnits tbentry : tbentries) {
					
					// Locate every Table B entry which has BUFRUnitOld as its Units field. 
					//getEN = cn.prepareStatement("SELECT id FROM bufr_b WHERE bufr_unit LIKE ?");
			    	//getEN.setString(1, tbentry.getBUFRUnitOld().concat("%"));
			    	getEN = cn.prepareStatement("SELECT id FROM bufr_b WHERE bufr_unit = ?");
			    	getEN.setString(1, tbentry.getBUFRUnitOld());
			    	ResultSet rs_getEN = getEN.executeQuery();
					nidx_ent = 0;
					while (rs_getEN.next()) {  
						idx_ent[nidx_ent++] = rs_getEN.getInt("id");
					}
				
					for ( int i = 0; i < nidx_ent; i++ ) {
						updateDB = cn.prepareStatement("UPDATE bufr_b SET bufr_unit = ? WHERE id = ?");
						updateDB.setString(1, tbentry.getBUFRUnitNew());
						updateDB.setInt(2, idx_ent[i]);
						fw.write(updateDB.toString() + "\n");   // Write copy of SQL command to output file.
						updateDB.executeUpdate();
					}
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no TableB UpdateUnits entries in the list!");
		}
		
	}

}

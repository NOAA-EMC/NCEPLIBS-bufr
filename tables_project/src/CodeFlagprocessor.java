import java.io.*;
import java.sql.*;
import java.util.*;

import CodeFlag.jabx.*;

public class CodeFlagprocessor {
	
	protected List<ExportingBCCodeFlagE> tbentries;

	//  Constructor methods.
	
	public CodeFlagprocessor() {
		tbentries.clear();
	}
	
	public CodeFlagprocessor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingBCCodeFlagE();
		}
		else if ( ! ( ob instanceof String ) ) {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all CodeFlag entries.
	
	public void writeList(FileWriter fw) {
		String fxy, fxyprev;
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				fxyprev = "";
				for (ExportingBCCodeFlagE tbentry : tbentries) {
			    	fxy = tbentry.getFXY();
			    	if ( ! fxyprev.equals(fxy) ) {  // Beginning of new CodeFlag entry.
			    		outline = String.format("\nEntry #%5.0f  %s-%s-%s %15s   >%s<\n",
			    				tbentry.getNo(),
			    				fxy.substring(0,1), fxy.substring(1,3), fxy.substring(3,6),
			    				tbentry.getStatus(), tbentry.getElementName() );
			    		fw.write(outline);
			    	}
			    	if ( tbentry.getCodeFigure() != null ) {
			    		outline = String.format("Entry #%5.0f  %6s  >%s<\n",
			    				tbentry.getNo(), tbentry.getCodeFigure(),
			    				tbentry.getDescription1() );
			    		fw.write(outline);
			    	}
			    	fxyprev = fxy;
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no CodeFlag entries in the list!");
		}
		
	}
	
	
	//  Method to ingest all CodeFlag entries into the database.
	
	public void ingest(FileWriter fw, Connection cn) {
		String fxy, fxyprev;
		PreparedStatement getEN, updateDB1, updateDB2;
		String sqlstg_getEN = "SELECT EntryNumber, Units FROM B_DESCRIPTORS WHERE X = ? AND Y = ?";
		String[] sqlstg_updateDB1 = {
				"INSERT INTO CODETABLE_ENTRIES (Value, Meaning) VALUES(?, ?)",
				"INSERT INTO FLAGTABLE_ENTRIES (BitNumber, Meaning) VALUES(?, ?)"
				};
		String[] sqlstg_updateDB2 = {
				"INSERT INTO B_TO_CT_RELATIONSHIPS (B_EntryNumber, CodeTable_EntryNumber) VALUES(?, ?)",
		        "INSERT INTO B_TO_FT_RELATIONSHIPS (B_EntryNumber, FlagTable_EntryNumber) VALUES(?, ?)"
				};
		ResultSet rs_getEN = null;
		String tbCF, tbDesc;
		int idx_stg = 0;
		int[] tablectr = { 0, 0 };
		int[] idx_ent = new int[3];
		int nidx_ent = 0;
		
		if ( tbentries.size() > 0 ) {
			try {
				fxyprev = "";
				for (ExportingBCCodeFlagE tbentry : tbentries) {
			    	fxy = tbentry.getFXY();
			    	if ( ! fxyprev.equals(fxy) ) {
			    		/*
			    		 *  This is the beginning of a new CodeFlag entry, so we need to locate all instances of the
			    		 *  corresponding FXY number within TableB.
			    		 *  
			    		 *  The PostGres JDBC driver doesn't seem to support scrollable ResultSets, so we can only
			    		 *  go through the rs_getEN ResultSet one time, and thus we need to store information
			    		 *  in some corresponding indices and arrays for later use.
			    		 */
			    		getEN = cn.prepareStatement(sqlstg_getEN);
				    	getEN.setInt(1, Integer.parseInt(fxy.substring(1,3)));
				    	getEN.setInt(2, Integer.parseInt(fxy.substring(3,6)));
				    	System.out.println(getEN.toString());
						rs_getEN = getEN.executeQuery();
						nidx_ent = 0;
		    			while (rs_getEN.next()) {  
		    				if (rs_getEN.getString("Units").contains("Code")) {
								idx_stg = 0;
							}
							else {
								idx_stg = 1;
							}
		    				System.out.println("idx_stg nidx_ent = " + String.valueOf(idx_stg) + " " + String.valueOf(nidx_ent));
		    				idx_ent[nidx_ent++] = rs_getEN.getInt("EntryNumber");
		    				System.out.println("idx_ent[nidx_ent] = " + String.valueOf(idx_ent[nidx_ent-1]));
						}
			    	}
			    	tbCF = tbentry.getCodeFigure();
			    	tbDesc = tbentry.getDescription1();
			    	if ( ( tbCF != null ) && ( tbDesc != null ) ) {
			    		if ( ( ! tbDesc.startsWith("Missing ") ) &&
			    			 ( ! tbDesc.startsWith("Reserved") ) &&
			    			 ( ! tbDesc.startsWith("Not used") ) &&
			    			 ( ! tbCF.contains("-") ) &&
			    			 ( ! tbCF.trim().isEmpty() ) ) {
			    			/*
			    			 *   Insert this entry into the appropriate table (i.e. Code or Flag).
			    			 */
			    			updateDB1 = cn.prepareStatement(sqlstg_updateDB1[idx_stg]);
			    			updateDB1.setInt(1, Integer.parseInt(tbCF.trim()));
			    			updateDB1.setString(2, tbDesc);
			    			fw.write(updateDB1.toString() + "\n");  // Write copy of SQL command to output file.
			    			updateDB1.executeUpdate();
			    			/*
			    			 *   Now, we need to make a corresponding entry in the appropriate relationships table
			    			 *   (i.e. "B_to_CT" or "B_to_FT") for all instances of the corresponding FXY number
			    			 *   in TableB.
			    			 */
			    			tablectr[idx_stg]++;
			    			for ( int i = 0; i < nidx_ent; i++ ) {
				    			updateDB2 = cn.prepareStatement(sqlstg_updateDB2[idx_stg]);
				    			updateDB2.setInt(1, idx_ent[i] );
				    			updateDB2.setInt(2, tablectr[idx_stg]);
				    			fw.write(updateDB2.toString() + "\n");  // Write copy of SQL command to output file.
				    			updateDB2.executeUpdate();
				    		}
			    			
			    		}
			    		
			    	}
			    	fxyprev = fxy;
				}
			}
			
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		
		else {
			System.out.println("There were no CodeFlag entries in the list!");
		}
		

	}
	
	
	/*
	 *   Method to create CodeFlag entries for a particular version number, by copying them from the previous version
	 *   number and then adjusting the bufr_b_idx and depends_on fields as appropriate for the newer version.
	 *   
	 *   The advantage of this approach is that it will include all of the Common Code Table entries as well as
	 *   corrected/updated meaning fields from the previous version (otherwise we'd have to manually add and correct/update
	 *   these in each new version after reading in the XML files from WMO!).
	 *   
	 *   The only disadvantage is that, for each new version, we need to manually add in any new code/flag tables
	 *   and/or new entries by hand after running this method (and before running this method again for the next subsequent
	 *   version!), but that seems like a worthwhile tradeoff and shouldn't be too difficult using a MSWord or PDF copy of
	 *   the code/flag tables from the WMO web site.
	 */
	
	public void copy_and_ingest(FileWriter fw, Connection cn, Integer vn, Integer ven) {
		PreparedStatement getDpOn, getDpOn2, getPrevCF, updateDB;
		String sqlstg_getDpOn = "SELECT cf.value, cf.bitnumber, b.fxy FROM code_flag_lookup cf " +
				"JOIN bufr_b b ON b.id = cf.bufr_b_idx WHERE cf.id = ?";
		String[] sqlstg_getDpOn2 = {
				"SELECT cf.id FROM code_flag_lookup cf JOIN bufr_b b ON b.id = cf.bufr_b_idx " +
					"WHERE b.version_idx = ? AND b.fxy = ? AND cf.value = ?",
				"SELECT cf.id FROM code_flag_lookup cf JOIN bufr_b b ON b.id = cf.bufr_b_idx " +
					"WHERE b.version_idx = ? AND b.fxy = ? AND cf.bitnumber = ?"
				};
		String sqlstg_getPrevCF = "SELECT cf.value, cf.bitnumber, cf.meaning, cf.depends_on, b.fxy " +
				"FROM code_flag_lookup cf JOIN bufr_b b ON b.id = cf.bufr_b_idx " +
				"WHERE b.version_idx = (SELECT id FROM versions WHERE versionnumber = ? AND mastertable = 0) " +
				"ORDER BY cf.depends_on, b.fxy";
				// We need the entries where depends_on is NOT NULL to be at the end of this result set.  This guarantees that
				// the entries on which they depend will get INSERT'ed into the code_flag_lookup table first, and thereby allow
				// them to be referenced by the depends_on foreign key within the new version.
		String[] sqlstg_updateDB = {
				"INSERT INTO code_flag_lookup (value, meaning, bufr_b_idx) VALUES(?, ?, ?)",
				"INSERT INTO code_flag_lookup (bitnumber, meaning, bufr_b_idx) VALUES(?, ?, ?)",
				"INSERT INTO code_flag_lookup (value, meaning, bufr_b_idx, depends_on) VALUES(?, ?, ?, ?)",
				"INSERT INTO code_flag_lookup (bitnumber, meaning, bufr_b_idx, depends_on) VALUES(?, ?, ?, ?)"
				};
		ResultSet rs_getDpOn, rs_getDpOn2, rs_getPrevCF = null;
		int idx_ustg, idx_gstg, valbin, depends_on;
		
		EntryNumberFinder enf = new EntryNumberFinder (fw, cn, ven);
		
		try {
			getPrevCF = cn.prepareStatement(sqlstg_getPrevCF);
			getPrevCF.setInt(1, vn-1);
			//fw.write(getPrevCF.toString() + "\n");
			rs_getPrevCF = getPrevCF.executeQuery();
			while (rs_getPrevCF.next()) {  // For each code_flag_lookup entry in the previous table version...
				
				idx_ustg = ( ( rs_getPrevCF.getString("value") != null ) ? 0 : 1 );
				if ( rs_getPrevCF.getString("depends_on") != null ) idx_ustg += 2;
				updateDB = cn.prepareStatement(sqlstg_updateDB[idx_ustg]);
				
				valbin = ( ( idx_ustg % 2 == 0 ) ? rs_getPrevCF.getInt("value") : rs_getPrevCF.getInt("bitnumber") );
				updateDB.setInt(1, valbin);
				updateDB.setString(2, rs_getPrevCF.getString("meaning"));
				updateDB.setInt(3, enf.getEntryNumber("bufr_b", rs_getPrevCF.getString("fxy")));

				if ( rs_getPrevCF.getString("depends_on") != null ) {
					getDpOn = cn.prepareStatement(sqlstg_getDpOn);
					getDpOn.setInt(1, rs_getPrevCF.getInt("depends_on"));
					rs_getDpOn = getDpOn.executeQuery();
					rs_getDpOn.next();
					if ( rs_getDpOn.getString("value") != null ) {
						idx_gstg = 0;
						valbin = rs_getDpOn.getInt("value");
					}
					else {
						idx_gstg = 1;
						valbin = rs_getDpOn.getInt("bitnumber");
					}
					getDpOn2 = cn.prepareStatement(sqlstg_getDpOn2[idx_gstg]);
					getDpOn2.setInt(1, ven);
					getDpOn2.setString(2, rs_getDpOn.getString("fxy"));
					getDpOn2.setInt(3, valbin);
					//fw.write(getDpOn2.toString() + "\n");
					rs_getDpOn2 = getDpOn2.executeQuery();
					depends_on = ( rs_getDpOn2.next() ? rs_getDpOn2.getInt("id") : 0 );
					updateDB.setInt(4, depends_on);
				}
				
    			fw.write(updateDB.toString() + "\n");  // Write copy of SQL command to output file.
    			updateDB.executeUpdate();

			}
		}
		
	    catch(Exception ex) {
	    	ex.printStackTrace();
	    }	
	

	}
	

}


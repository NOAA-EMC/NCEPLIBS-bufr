import java.io.*;
import java.sql.*;

public class CCTprocessor {  // for use within all CommonCodeTable processors

	protected FileWriter myfw;
	protected Connection myconn;
	
	protected PreparedStatement getEN;
	protected int[] idx_ent = new int[3];
	protected int nidx_ent;
	
	
	//  Constructor method for Common Code Table C13.
	
	public CCTprocessor (FileWriter fw, Connection conn, Integer category) {
		
		myfw = fw;
		myconn = conn;
		
		try {
			// Locate all instances of the corresponding category number within Table A. 
			getEN = myconn.prepareStatement("SELECT EntryNumber FROM A_DESCRIPTORS WHERE Value = ?");
	    	getEN.setInt(1, category);
	    	ResultSet rs_getEN = getEN.executeQuery();
			nidx_ent = 0;
			while (rs_getEN.next()) {  
				idx_ent[nidx_ent++] = rs_getEN.getInt("EntryNumber");
			}
		}
		catch(Exception ex) {
	    	ex.printStackTrace();
		}
	}
	
	//  Constructor method for all Common Code Tables except C13.
	
	public CCTprocessor (FileWriter fw, Connection conn, Integer x, Integer y) {
		
		myfw = fw;
		myconn = conn;
		
		try {
			// Locate all instances of the corresponding FXY number within TableB. 
			getEN = myconn.prepareStatement("SELECT EntryNumber FROM B_DESCRIPTORS WHERE X = ? AND Y = ?");
	    	getEN.setInt(1, x);
	    	getEN.setInt(2, y);
	    	ResultSet rs_getEN = getEN.executeQuery();
			nidx_ent = 0;
			while (rs_getEN.next()) {  
				idx_ent[nidx_ent++] = rs_getEN.getInt("EntryNumber");
			}
		}
		catch(Exception ex) {
	    	ex.printStackTrace();
		}
	}
	
	/*
	 *   Method to ingest a new CommonCodeTableC12 entry into the CodeTable_Entries and
	 *   B_to_CT_Relationships tables.
	 */
	
	public void ingest (String value, String meaning, int C11ctEN, int [] C11EN, int nC11EN ) {
		try {
			if ( ( value != null ) && ( meaning != null ) ) {
					
					// Insert new entry into CodeTable_Entries
					PreparedStatement updateDB1 = myconn.prepareStatement
									("INSERT INTO CODETABLE_ENTRIES (Value, Meaning) VALUES(?, ?)");
					updateDB1.setInt(1, Integer.parseInt(value));
					updateDB1.setString(2, meaning);
					myfw.write(updateDB1.toString() + "\n");   // Write copy of SQL command to output file.
					updateDB1.executeUpdate();
	
					// Get the EntryNumber associated with this new entry.
					Integer ctEN = 0;  // set default value
					getEN = myconn.prepareStatement("SELECT CURRVAL('CT_ENT_SEQ');");
			    	ResultSet rs_getEN = getEN.executeQuery();
					while (rs_getEN.next()) {  
						ctEN = Integer.parseInt(rs_getEN.getString(1).trim());
					}
					
					// Insert corresponding entries into B_to_CT_Relationships
					for ( int j = 0; j < nC11EN; j++ ) {
						for ( int i = 0; i < nidx_ent; i++ ) {
							if ( j % 2 == i ) {
								PreparedStatement updateDB2 = myconn.prepareStatement
								    ("INSERT INTO B_TO_CT_RELATIONSHIPS (B_EntryNumber, CodeTable_EntryNumber, Depends_On) " +
								     "VALUES ( ?, ?, (SELECT EntryNumber FROM B_TO_CT_RELATIONSHIPS WHERE " +
								    		          "B_EntryNumber = ? AND CodeTable_EntryNumber = ?) )");
								updateDB2.setInt(1, idx_ent[i]);
								updateDB2.setInt(2, ctEN);
								updateDB2.setInt(3, C11EN[j]);
								updateDB2.setInt(4, C11ctEN);
								myfw.write(updateDB2.toString() + "\n");  // Write copy of SQL command to output file.
				    			updateDB2.executeUpdate();
							}
						}
					}
			}
		}
		catch(Exception ex) {
	    	ex.printStackTrace();
	    }

	}
	
	/*
	 *   Method to ingest a new CommonCodeTableC13 entry into the CodeTable_Entries and
	 *   A_to_CT_Relationships tables.
	 */
	public void ingestC13 (String value, String meaning) {
		
		try {
			if ( ( value != null ) && ( meaning != null ) ) {
				if ( ( ! meaning.startsWith("Missing ") ) &&
		    		 ( ! meaning.startsWith("Reserved") ) &&
		    		 ( ! meaning.startsWith("Not used") ) &&
		    		 ( ! value.contains("-") ) &&
		    		 ( ! value.trim().isEmpty() ) ) {
					
					// Insert new entry into CodeTable_Entries
					PreparedStatement updateDB1 = myconn.prepareStatement
									("INSERT INTO CODETABLE_ENTRIES (Value, Meaning) VALUES(?, ?)");
					updateDB1.setInt(1, Integer.parseInt(value));
					updateDB1.setString(2, meaning);
					myfw.write(updateDB1.toString() + "\n");   // Write copy of SQL command to output file.
					updateDB1.executeUpdate();
					
					// Get the EntryNumber associated with this new entry.
					Integer ctEN = 0;  // set default value
					getEN = myconn.prepareStatement("SELECT CURRVAL('CT_ENT_SEQ');");
			    	ResultSet rs_getEN = getEN.executeQuery();
					while (rs_getEN.next()) {  
						ctEN = Integer.parseInt(rs_getEN.getString(1).trim());
					}
					
					// Insert corresponding entries into B_to_CT_Relationships
					PreparedStatement updateDB2 = myconn.prepareStatement
									("INSERT INTO A_TO_CT_RELATIONSHIPS (A_EntryNumber, CodeTable_EntryNumber) VALUES(?, ?)");
					for ( int i = 0; i < nidx_ent; i++ ) {
		    			updateDB2.setInt(1, idx_ent[i] );
		    			updateDB2.setInt(2, ctEN);
		    			myfw.write(updateDB2.toString() + "\n");  // Write copy of SQL command to output file.
		    			updateDB2.executeUpdate();
		    		}
				}
			}
		}
		
		catch(Exception ex) {
	    	ex.printStackTrace();
	    }

	}
	/*
	 *   Method to ingest a new CommonCodeTableCx entry (where x != 12 and x != 13) into the CodeTable_Entries and
	 *   B_to_CT_Relationships tables.
	 */
	
	public void ingest (String value, String meaning) {
		
		try {
			if ( ( value != null ) && ( meaning != null ) ) {
				if ( ( ! meaning.startsWith("Missing ") ) &&
		    		 ( ! meaning.startsWith("Reserved") ) &&
		    		 ( ! meaning.startsWith("Not used") ) &&
		    		 ( ! value.contains("-") ) &&
		    		 ( ! value.trim().isEmpty() ) ) {
					
					// Insert new entry into CodeTable_Entries
					PreparedStatement updateDB1 = myconn.prepareStatement
									("INSERT INTO CODETABLE_ENTRIES (Value, Meaning) VALUES(?, ?)");
					updateDB1.setInt(1, Integer.parseInt(value));
					updateDB1.setString(2, meaning);
					myfw.write(updateDB1.toString() + "\n");   // Write copy of SQL command to output file.
					updateDB1.executeUpdate();
					
					// Get the EntryNumber associated with this new entry.
					Integer ctEN = 0;  // set default value
					getEN = myconn.prepareStatement("SELECT CURRVAL('CT_ENT_SEQ');");
			    	ResultSet rs_getEN = getEN.executeQuery();
					while (rs_getEN.next()) {  
						ctEN = Integer.parseInt(rs_getEN.getString(1).trim());
					}
					// Insert corresponding entries into B_to_CT_Relationships
					PreparedStatement updateDB2 = myconn.prepareStatement
									("INSERT INTO B_TO_CT_RELATIONSHIPS (B_EntryNumber, CodeTable_EntryNumber) VALUES(?, ?)");
					for ( int i = 0; i < nidx_ent; i++ ) {
		    			updateDB2.setInt(1, idx_ent[i] );
		    			updateDB2.setInt(2, ctEN);
		    			myfw.write(updateDB2.toString() + "\n");  // Write copy of SQL command to output file.
		    			updateDB2.executeUpdate();
		    		}
				}
			}
		}
		
		catch(Exception ex) {
	    	ex.printStackTrace();
	    }

	}
	
}

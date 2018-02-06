package IDfinder;

import java.sql.*;

public class IDfinder {

	// Given a master table number and table version number (and, optionally, an originating center number as well),
	// find the associated id number (primary key) in the versions table.
	
	protected Integer versionID = -1; // Set -1 as the default value in case a corresponding entry can't be found in the table.
	
	protected String sqlstg = "SELECT id FROM versions WHERE versionnumber = ? AND mastertable = ?";
	
	// Constructor methods.
	
	public IDfinder (Connection conn, Integer versionNumber, Integer masterTable ) {
		try {
			PreparedStatement getEN = conn.prepareStatement(sqlstg);
	    	getEN.setInt(1, versionNumber);
	    	getEN.setInt(2, masterTable);
			ResultSet rs = getEN.executeQuery();
			while (rs.next()) {
				versionID = rs.getInt("id");
			}
		}
		catch(Exception ex) {
	    	ex.printStackTrace();
		}
	}
	
	public IDfinder (Connection conn, Integer versionNumber, Integer masterTable, Integer originatingCenter ) {
		try {
			PreparedStatement getEN = conn.prepareStatement(sqlstg.concat(" AND originatingcenter = ?"));
	    	getEN.setInt(1, versionNumber);
	    	getEN.setInt(2, masterTable);
	    	getEN.setInt(3, originatingCenter);
			ResultSet rs = getEN.executeQuery();
			while (rs.next()) {
				versionID = rs.getInt("id");
			}
		}
		catch(Exception ex) {
	    	ex.printStackTrace();
		}
	}

	// Getter method.
		
	public Integer getVersionID() {
		return versionID;
	}
	
}

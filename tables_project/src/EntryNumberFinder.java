import java.io.*;
import java.sql.*;
import descriptorAnalyzer.*;

public class EntryNumberFinder {

	protected FileWriter myfw;
	protected Connection myconn;
	protected Integer myven;

	//  Constructor method.
	
	public EntryNumberFinder (FileWriter fw, Connection conn, Integer ven) {
		myfw = fw;
		myconn = conn;
		myven = ven;
	}
	
	/*
	 *   Method to find and return the EntryNumber corresponding to fxy and ven from within the table defined by tableName.
	 *   A return value of -1 indicates failure.
	 */
	
	public Integer getEntryNumber (String tableName, String fxy) {
		
		String sqlstg;
		PreparedStatement getEN;
		ResultSet rs;
		DescriptorAnalyzer da = new DescriptorAnalyzer();
		
		Integer tbEN, f, x, y, count;
		
		try {
			
			tbEN = -1;  // set default return value to indicate failure
			
			sqlstg = "SELECT id FROM " + tableName + " WHERE f = ? AND x = ? AND y = ? AND version_idx = ?";
			
			f = Integer.parseInt(da.getF(fxy));
			x = Integer.parseInt(da.getX(fxy));
			y = Integer.parseInt(da.getY(fxy));
			
			if ( f == 0 || f == 3 ) {
				getEN = myconn.prepareStatement(sqlstg);
				getEN.setInt(1, f);
				getEN.setInt(2, x);
				getEN.setInt(3, y);
				getEN.setInt(4, myven);
				//myfw.write(getEN.toString() + "\n");
				rs = getEN.executeQuery();
				
				count = 0;
				while (rs.next()) {
					count++;
					tbEN = rs.getInt("id");
				}
			
				if ( f == 0 ) {
					if ( count == 1 ) return tbEN;  // there should be (at most) one matching entry!
				}
				else {  // ( f == 3 )
					if ( count >= 1 ) return tbEN;  // there may be multiple matching entries, so return the id of the last
				}									// one in the result set
				
			}
			
		}
		
		catch(Exception ex) {
	    	ex.printStackTrace();
	    }
		
		return -1;  // If we reached this point, then we were unable to find the EntryNumber we were searching for.
	}
}
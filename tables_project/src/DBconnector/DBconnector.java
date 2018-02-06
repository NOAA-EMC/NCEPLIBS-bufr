package DBconnector;

import java.sql.*;

public class DBconnector {

	protected Connection myconn;
	
	public DBconnector () {
		try {
			Class.forName("com.mysql.jdbc.Driver").newInstance();
	    	String dbURL = "jdbc:mysql://ncointra.ncep.noaa.gov/nco_bufr_tables";
	    	myconn = DriverManager.getConnection(dbURL, "XXXXX", "XXXXX");
		}
	    catch(Exception ex) {
	    	ex.printStackTrace();
	    }
	}
	
	public Connection getConnection ( ) {
	    return myconn;
	}
}

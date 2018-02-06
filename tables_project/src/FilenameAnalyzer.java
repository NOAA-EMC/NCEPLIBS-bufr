public class FilenameAnalyzer {

	protected String tableType;
	protected Integer originatingCenter;
	protected Integer versionNumber;
	protected Integer masterTable;

	//  Constructor method.
	
	public FilenameAnalyzer (String filename) {
		
		//  Set default values.
	    tableType = "";
	    originatingCenter = -1;
	    versionNumber = -1;
	    masterTable = -1;
	    
	    String dfbasename = filename.substring(filename.lastIndexOf("/")+1,filename.lastIndexOf("."));
	    String[] quals = dfbasename.split("_");

	    //  Determine the table type.  If this is a local table, also determine the originating center.
	    if ( quals[0].startsWith("CommonCodeTable") ) {
	    	tableType = quals[0].substring(quals[0].lastIndexOf("C"));
	    }
	    else if ( quals[0].startsWith("LOCAL") ) {
	    	tableType = quals[0];
	    	if ( quals.length > 2 ) originatingCenter = Integer.parseInt(quals[2]);
	    }
	    else if ( quals[0].startsWith("BUFR") ) {
	    	tableType = quals[4];
	    }
	    else if ( quals[0].startsWith("BC") ) {
	    	tableType = quals[0].substring(2);
	    }
	    else if ( quals[0].startsWith("B") ) {
	    	tableType = quals[0].substring(1);
	    }
	    
	    //  Determine the version number.
	    if (  quals.length > 1 ) {
	    	if (quals[1].startsWith("BUFR")) {
	    		versionNumber = Integer.parseInt(quals[1].substring(quals[1].lastIndexOf("R")+1));
	    	}
	    	else if (quals[0].startsWith("BUFR")) {
	    		versionNumber = Integer.parseInt(quals[1]);
	    	}
	    }
	    
	    //  Determine the master table number.
	    if ( quals.length > 3 ) masterTable = Integer.parseInt(quals[3]);
	}
	
	//  Getter methods.
	
	public String getTableType() {
		return tableType;
	}
	
	public Integer getOriginatingCenter() {
		return originatingCenter;
	}
	
	public Integer getVersionNumber() {
		return versionNumber;
	}

	public Integer getMasterTable() {
		return masterTable;
	}
	
}
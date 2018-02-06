import java.io.*;
import java.sql.*;
import java.util.*;

import C11.jabx.*;

public class C11processor {
	
	protected List<ExportingCommonCodeTableC11E> tbentries;

	//  Constructor methods.
	
	public C11processor() {
		tbentries.clear();
	}
	
	public C11processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC11E();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Common C11 entries.
	
	public void writeList(FileWriter fw) {
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingCommonCodeTableC11E tbentry : tbentries) {
			    	outline = String.format("Entry #%5d %5s %15s  >%s<\n",
			    			tbentry.getNo(), tbentry.getGRIB2BUFR4(),
			    			tbentry.getStatus(), tbentry.getName());
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C11 entries in the list!");
		}
		
	}
	
	
	//  Method to ingest all Common C11 entries.
	
	public void ingest(FileWriter fw, Connection conn) {
		
		if ( tbentries.size() > 0 ) {
			try {
				CCTprocessor cctp = new CCTprocessor (fw, conn, 1, 35); // C11 is the code table for 0-01-035
				for (ExportingCommonCodeTableC11E tbentry : tbentries) {
					cctp.ingest(tbentry.getGRIB2BUFR4(), tbentry.getName());
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C11 entries in the list!");
		}
		
	}

}

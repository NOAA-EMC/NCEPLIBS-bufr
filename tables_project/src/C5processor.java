import java.io.*;
import java.sql.*;
import java.util.*;

import C5.jabx.*;

public class C5processor {
	
	protected List<ExportingCommonCodeTableC5E> tbentries;

	//  Constructor methods.
	
	public C5processor() {
		tbentries.clear();
	}
	
	public C5processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC5E();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Common C5 entries.
	
	public void writeList(FileWriter fw) {
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingCommonCodeTableC5E tbentry : tbentries) {
			    	outline = String.format("Entry #%5d %5s %15s  >%s<\n",
			    			tbentry.getNo(), tbentry.getBUFR(),
			    			tbentry.getStatus(), tbentry.getName());
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C5 entries in the list!");
		}
		
	}
	
	
	//  Method to ingest all Common C5 entries.
	
	public void ingest(FileWriter fw, Connection conn) {
		
		if ( tbentries.size() > 0 ) {
			try {
				CCTprocessor cctp = new CCTprocessor (fw, conn, 1, 7); // C5 is the code table for 0-01-007
				for (ExportingCommonCodeTableC5E tbentry : tbentries) {
					cctp.ingest(tbentry.getBUFR(), tbentry.getName());
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C5 entries in the list!");
		}
		
	}

}

import java.io.*;
import java.sql.*;
import java.util.*;

import C4.jabx.*;

public class C4processor {
	
	protected List<ExportingCommonCodeTableC4E> tbentries;

	//  Constructor methods.
	
	public C4processor() {
		tbentries.clear();
	}
	
	public C4processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC4E();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Common C4 entries.
	
	public void writeList(FileWriter fw) {
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingCommonCodeTableC4E tbentry : tbentries) {
			    	outline = String.format("Entry #%5d %5s %15s  >%s<\n",
			    			tbentry.getNo(), tbentry.getBUFR(),
			    			tbentry.getStatus(), tbentry.getMeaning());
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C4 entries in the list!");
		}
		
	}
	
	
	//  Method to ingest all Common C4 entries.
	
	public void ingest(FileWriter fw, Connection conn) {
		
		if ( tbentries.size() > 0 ) {
			try {
				CCTprocessor cctp = new CCTprocessor (fw, conn, 22, 68); // C4 is the code table for 0-22-068
				for (ExportingCommonCodeTableC4E tbentry : tbentries) {
					cctp.ingest(tbentry.getBUFR(), tbentry.getMeaning());
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C4 entries in the list!");
		}
		
	}

}

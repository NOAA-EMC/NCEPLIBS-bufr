import java.io.*;
import java.sql.*;
import java.util.*;

import C7.jabx.*;

public class C7processor {
	
	protected List<ExportingCommonCodeTableC7E> tbentries;

	//  Constructor methods.
	
	public C7processor() {
		tbentries.clear();
	}
	
	public C7processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC7E();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Common C7 entries.
	
	public void writeList(FileWriter fw) {
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingCommonCodeTableC7E tbentry : tbentries) {
			    	outline = String.format("Entry #%5d %5s %15s  >%s<\n",
			    			tbentry.getNo(), tbentry.getBUFR(),
			    			tbentry.getStatus(), tbentry.getTrackingTechniquesStatusOfSystem());
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C7 entries in the list!");
		}
		
	}
	
	
	//  Method to ingest all Common C7 entries.
	
	public void ingest(FileWriter fw, Connection conn) {
		
		if ( tbentries.size() > 0 ) {
			try {
				CCTprocessor cctp = new CCTprocessor (fw, conn, 2, 14); // C7 is the code table for 0-02-014
				for (ExportingCommonCodeTableC7E tbentry : tbentries) {
					cctp.ingest(tbentry.getBUFR(), tbentry.getTrackingTechniquesStatusOfSystem());
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C7 entries in the list!");
		}
		
	}

}

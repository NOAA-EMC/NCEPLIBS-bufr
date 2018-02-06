import java.io.*;
import java.sql.*;
import java.util.*;

import C2.jabx.*;

public class C2processor {
	
	protected List<ExportingCommonCodeTableC2E> tbentries;

	//  Constructor methods.
	
	public C2processor() {
		tbentries.clear();
	}
	
	public C2processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC2E();
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
				for (ExportingCommonCodeTableC2E tbentry : tbentries) {
			    	outline = String.format("Entry #%5d %5s %15s  >%s<\n",
			    			tbentry.getNo(), tbentry.getBUFR(),
			    			tbentry.getStatus(), tbentry.getRadiosondeSoundingSystemUsedE());
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C2 entries in the list!");
		}
		
	}
	
	
	//  Method to ingest all Common C2 entries.
	
	public void ingest(FileWriter fw, Connection conn) {
		
		if ( tbentries.size() > 0 ) {
			try {
				CCTprocessor cctp = new CCTprocessor (fw, conn, 2, 11); // C2 is the code table for 0-02-011
				for (ExportingCommonCodeTableC2E tbentry : tbentries) {
					cctp.ingest(tbentry.getBUFR(), tbentry.getRadiosondeSoundingSystemUsedE());
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C2 entries in the list!");
		}
		
	}

}

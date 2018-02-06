import java.io.*;
import java.sql.*;
import java.util.*;

import C13.jabx.*;

public class C13processor {
	
	protected List<ExportingCommonCodeTableC13E> tbentries;

	//  Constructor methods.
	
	public C13processor() {
		tbentries.clear();
	}
	
	public C13processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC13E();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Common C13 entries.
	
	public void writeList(FileWriter fw) {
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingCommonCodeTableC13E tbentry : tbentries) {
			    	outline = String.format("Entry #%3d  >%s<  >%s<  >%s<\n",
			    			tbentry.getNo(), tbentry.getCodeFigureCategory(),
			    			tbentry.getCodeFigureSubcategory(), tbentry.getNameSubcategory());
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C8 entries in the list!");
		}
		
	}
	
	
	//  Method to ingest all Common C13 entries.
	
	
	public void ingest(FileWriter fw, Connection conn) {
		Integer catg, lastcatg;
		CCTprocessor cctp;
		
		if ( tbentries.size() > 0 ) {
			try {
				lastcatg = 0;
				cctp = new CCTprocessor( fw, conn, lastcatg );
				for (ExportingCommonCodeTableC13E tbentry : tbentries) {
					catg = Integer.parseInt(tbentry.getCodeFigureCategory());
					if ( catg != lastcatg ) {  // initiate a new CCTprocessor object
							cctp = new CCTprocessor ( fw, conn, catg );
					}
					cctp.ingestC13(tbentry.getCodeFigureSubcategory(), tbentry.getNameSubcategory());
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C13 entries in the list!");
		}
		
	}

}

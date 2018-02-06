import java.io.*;
import java.sql.*;
import java.util.*;

import C12.jabx.*;

public class C12processor {
	
	protected List<ExportingCommonCodeTableC12E> tbentries;

	//  Constructor methods.
	
	public C12processor() {
		tbentries.clear();
	}
	
	public C12processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC12E();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Common C12 entries.
	
	public void writeList(FileWriter fw) {
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingCommonCodeTableC12E tbentry : tbentries) {
			    	outline = String.format("Entry #%3d %4s %15s  >%s<  >%s<\n",
			    			tbentry.getNo(), tbentry.getCodeFigureOriginatingCentre(),
			    			tbentry.getStatus(), tbentry.getCodeFigureSubCentre(), tbentry.getNameSubCentre() );
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C12 entries in the list!");
		}
		
	}
	
	
	//  Method to ingest all Common C12 entries.
	
	public void ingest(FileWriter fw, Connection conn) {

		if ( tbentries.size() > 0 ) {
			try {
				CCTprocessor cctp = new CCTprocessor (fw, conn, 1, 34); // C12 is the code table for 0-01-034
				
				int [] C11EN = new int [6]; // Note there are 3 different Table B descriptors corresponding
				                            // to Common Code Table C11, and 2 table versions (13 and 14) for each.
				EntryNumberFinder enf1 = new EntryNumberFinder(fw, conn, 1);
				EntryNumberFinder enf2 = new EntryNumberFinder(fw, conn, 2);
				C11EN[0] = enf1.getEntryNumber("B_Descriptors", "001031");
				C11EN[1] = enf2.getEntryNumber("B_Descriptors", "001031");
				C11EN[2] = enf1.getEntryNumber("B_Descriptors", "001033");
				C11EN[3] = enf2.getEntryNumber("B_Descriptors", "001033");
				C11EN[4] = enf1.getEntryNumber("B_Descriptors", "001035");
				C11EN[5] = enf2.getEntryNumber("B_Descriptors", "001035");
			
				for (ExportingCommonCodeTableC12E tbentry : tbentries) {
					if ( tbentry.getCodeFigureSubCentre() != null ) {
						// Get the C11 entry number associated with this C12 entry.
						PreparedStatement getEN = conn.prepareStatement
							("SELECT EntryNumber FROM CODETABLE_ENTRIES WHERE Value = ? AND MEANING = ?");
						getEN.setInt(1, Integer.parseInt(tbentry.getCodeFigureOriginatingCentre()));
						getEN.setString(2, tbentry.getNameOriginatingCentre());
						fw.write(getEN.toString() + "\n");
				    	ResultSet rs_getEN = getEN.executeQuery();
						if (rs_getEN.next()) {  
							cctp.ingest(tbentry.getCodeFigureSubCentre(), tbentry.getNameSubCentre(),
										Integer.parseInt(rs_getEN.getString(1).trim()), C11EN, 6);
						}
					}
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C12 entries in the list!");
		}
		
	}

}

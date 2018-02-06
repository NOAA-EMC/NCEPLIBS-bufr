import java.io.*;
import java.sql.*;
import java.util.*;

import C8.jabx.*;

public class C8processor {
	
	protected List<ExportingCommonCodeTableC8E> tbentries;

	//  Constructor methods.
	
	public C8processor() {
		tbentries.clear();
	}
	
	public C8processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC8E();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Common C8 entries.
	
	public void writeList(FileWriter fw) {
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingCommonCodeTableC8E tbentry : tbentries) {
			    	outline = String.format("Entry #%3d %4s %15s  >%s<  >%s<  >%s<  >%s<\n",
			    			tbentry.getNo(), tbentry.getCode(),
			    			tbentry.getStatus(), tbentry.getAgency(), tbentry.getType(),
			    			tbentry.getInstrumentShortName(), tbentry.getInstrumentLongName());
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
	
	
	//  Method to ingest all Common C8 entries.
	
	public void ingest(FileWriter fw, Connection conn) {
		String meaning;
		
		if ( tbentries.size() > 0 ) {
			try {
				CCTprocessor cctp = new CCTprocessor (fw, conn, 2, 19); // C8 is the code table for 0-02-019
				for (ExportingCommonCodeTableC8E tbentry : tbentries) {
					if ( tbentry.getAgency() != null ) {
						meaning = tbentry.getAgency() + " " + tbentry.getType() + " " + tbentry.getInstrumentShortName();
						if ( tbentry.getInstrumentLongName() != null )  {
							meaning = meaning.concat(" (" + tbentry.getInstrumentLongName() + ")");
						}
						cctp.ingest(tbentry.getCode(), meaning);
					}
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

}

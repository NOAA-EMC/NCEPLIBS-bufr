import java.io.*;
import java.sql.*;
import java.util.*;

import C14.jabx.*;

public class C14processor {
	
	protected List<ExportingCommonCodeTableC14E> tbentries;

	//  Constructor methods.
	
	public C14processor() {
		tbentries.clear();
	}
	
	public C14processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC14E();
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
				for (ExportingCommonCodeTableC14E tbentry : tbentries) {
			    	outline = String.format("Entry #%6d %6s %15s  >%s<  >%s<\n",
			    			tbentry.getNo(), tbentry.getCodeFigure(),
			    			tbentry.getStatus(), tbentry.getMeaning(), tbentry.getChemicalFormula());
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C14 entries in the list!");
		}
		
	}
	
	
	//  Method to ingest all Common C14 entries.
	
	public void ingest(FileWriter fw, Connection conn) {
		String meaning;
		
		if ( tbentries.size() > 0 ) {
			try {
				CCTprocessor cctp = new CCTprocessor (fw, conn, 8, 46); // C14 is the code table for 0-08-046
				for (ExportingCommonCodeTableC14E tbentry : tbentries) {
					meaning = tbentry.getMeaning();
					if ( tbentry.getChemicalFormula() != null )  {
						meaning = meaning.concat(" (" + tbentry.getChemicalFormula() + ")");
					}
					cctp.ingest(tbentry.getCodeFigure(), meaning);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C14 entries in the list!");
		}
		
	}

}

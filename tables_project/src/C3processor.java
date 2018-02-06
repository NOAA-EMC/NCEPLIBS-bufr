import java.io.*;
import java.sql.*;
import java.util.*;

import C3.jabx.*;

public class C3processor {
	
	protected List<ExportingCommonCodeTableC3E> tbentries;

	//  Constructor methods.
	
	public C3processor() {
		tbentries.clear();
	}
	
	public C3processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingCommonCodeTableC3E();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Common C3 entries.
	
	public void writeList(FileWriter fw) {
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				for (ExportingCommonCodeTableC3E tbentry : tbentries) {
			    	outline = String.format("Entry #%3.0f %4s %15s  >%s<  >%s<  >%s<\n",
			    			tbentry.getNo(), tbentry.getBUFR(),
			    			tbentry.getStatus(), tbentry.getInstrumentX0020MakeX0020AndX0020Type(),
			    			tbentry.getEquationCoefficientsA(), tbentry.getEquationCoefficientsB());
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C3 entries in the list!");
		}
		
	}
	
	
	//  Method to write an ASCII listing of all Common C3 entries.
	
	public void ingest(FileWriter fw, Connection conn) {
		String meaning;
		
		if ( tbentries.size() > 0 ) {
			try {
				CCTprocessor cctp = new CCTprocessor (fw, conn, 22, 67); // C3 is the code table for 0-22-067
				for (ExportingCommonCodeTableC3E tbentry : tbentries) {
					meaning = tbentry.getInstrumentX0020MakeX0020AndX0020Type();
					if ( ( tbentry.getEquationCoefficientsA() != null )  &&
						 ( tbentry.getEquationCoefficientsB() != null )  && 
						 ( ! tbentry.getEquationCoefficientsA().startsWith("Not applicable") ) &&
						 ( ! tbentry.getEquationCoefficientsB().startsWith("Not applicable") ) ) {
						meaning = meaning.concat(" (fall rate equation coefficients: a = " +
							tbentry.getEquationCoefficientsA() + ", b = " +
							tbentry.getEquationCoefficientsB() + ")");
					}
					cctp.ingest(tbentry.getBUFR(), meaning);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no Common C3 entries in the list!");
		}
		
	}

}

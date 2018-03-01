package DBextract;

import java.io.*;
import java.sql.*;
import java.util.Date;
import java.text.*;
import java.util.*;

import DBconnector.*;
import IDfinder.*;
import descriptorAnalyzer.*;
import tableConsolidator.*;

public class DBextract {

	public static void main(String[] args) {

	    //  Check for correct number of arguments.
	    if ( ( args.length < 5 ) || ( args.length > 6 ) ) {
	    	System.out.println("\nUsage: java DBextract tabletype mastertable version outdir format [origcenter]\n");
	    	System.out.println("    where tabletype   is the type of the table to be extracted");
	    	System.out.println("          mastertable is the master table number of the table to be extracted");
	    	System.out.println("          version     is the version number of the table to be extracted");
	    	System.out.println("          outdir      is the [path/]directory where the extracted data will be written");
	    	System.out.println("          format      is the format for the extracted content");
	    	System.out.println("          origcenter  is the originating center number of the table to be extracted");
	    	System.exit(-1);
	    }
	    
    	//  Check validity of input arguments.
	    
	    Integer tabletype = 1;  // defaults to "Table A"
	    if ( args[0].equalsIgnoreCase("TableB") ) {
    		tabletype = 2;
    	}
    	else if ( args[0].equalsIgnoreCase("TableD") ) {
    		tabletype = 3;
    	}
    	else if ( args[0].equalsIgnoreCase("CodeFlag") ) {
    		tabletype = 4;
    	}
    	else if ( ! args[0].equalsIgnoreCase("TableA") ) {
    		System.out.println("Invalid tabletype: " + args[0]);
	    	System.exit(-2);
    	}
	    
	    Integer format = 1;  // defaults to "bufrlib"
    	if ( args[4].equalsIgnoreCase("melbufr") ) {
    		format = 2;
    	}
    	else if ( args[4].equalsIgnoreCase("html") ) {
    		format = 3;
    	}
    	else if ( ! args[4].equalsIgnoreCase("bufrlib") ){
	    	System.out.println("Invalid format: " + args[4]);
		    System.exit(-3);
	    }
    	
	    try {
	    	
	    	//  Open the database.
	    	DBconnector dbc = new DBconnector();
	    	Connection conn = dbc.getConnection();
	    	
	    	//  Open the output file and get the version ID number.
	    	FileWriter fw;
	    	IDfinder idf;
	    	String html_tbfn = "";
	    	Boolean tableSTD = ( args.length == 5 );  // are we generating a STD or LOC table?
	    	if ( tableSTD ) {
	    		idf = new IDfinder(conn, Integer.parseInt(args[2]), Integer.parseInt(args[1]) );
	    		switch ( format ) {
	    		case 1:	 // bufrlib
	    			fw = new FileWriter(args[3] + "/bufrtab." + args[0] + "_STD_" + args[1] + "_" + args[2] + "." + args[4]);
	    			break;
	    		case 2:  // melbufr
	    			if ( tabletype == 4 ) {  // CodeFlag
	    				fw = new FileWriter(args[3] + String.format("/B3M-%03d-%03d-0",
    								Integer.parseInt(args[1]), Integer.parseInt(args[2])));
	    			}
	    			else {  // Table A, B or D
	    				fw = new FileWriter(args[3] + String.format("/B3M-%03d-%03d-%c",
	    							Integer.parseInt(args[1]), Integer.parseInt(args[2]), args[0].charAt(5)));
	    			}
	    			break;
	    		default:  // html
	    			// for the NCEP web tables, store all of the WMO-standard and local (originating center 7) descriptors together in the same file
	    			html_tbfn = args[0] + "_" + args[1] + "_STDv" + args[2] + "_LOC7" + ".html";
	    			fw = new FileWriter(args[3] + "/" + html_tbfn);
	    			break;
	    		}
	    	}
	    	else {
	    		idf = new IDfinder(conn, Integer.parseInt(args[2]), Integer.parseInt(args[1]), Integer.parseInt(args[5]) );
	    		switch ( format ) {
	    		case 1:	 // bufrlib
	    			fw = new FileWriter(args[3] + "/bufrtab." + args[0] + "_LOC_" + args[1] + "_" + args[5] + "_" + args[2] + "." + args[4]);
	    			break;
	    		case 2:  // melbufr
	    			fw = new FileWriter(args[3] + String.format("/B3L-%03d-%03d-%c",
	    							Integer.parseInt(args[5]), Integer.parseInt(args[2]), args[0].charAt(5)));
	    			break;
	    		default: // html
	    			// we don't really need to do anything here, since the local (originating center 7) descriptors are already automatically
	    			// stored together with the standard descriptors in the NCEP web tables via the above logic; however, we do need to set a
	    			// placeholder value for fw here so that the compiler won't flag it as a possibly-undefined value in the below logic
	    			fw = new FileWriter(args[3] + "/" + args[0] + "_PLACEHOLDER_FILE.html");
	    			break;
	    		}
	    		
	    	}
	    	
	    	//  Declare some additional variables.
	    	String querystg, querystg2, querystg3, outline;
	    	PreparedStatement queryDB, queryDB2, queryDB3;
	    	ResultSet rs_queryDB, rs_queryDB2, rs_queryDB3;
	    	DescriptorAnalyzer da = new DescriptorAnalyzer();
	    	DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
	    	
	   	
	    	//  Write header to output file.
	    	String tbt = ( tabletype == 4 ? "CODE/FLAG" : args[0].substring(5) );
	    	if ( ( format == 1 ) && ( tabletype != 1 ) ) {  // bufrlib Table B, D or CodeFlag
	    		if ( tableSTD ) {
    				fw.write("Table " + ( tabletype == 4 ? "F" : tbt ) + " STD |  " + args[1] + " | " + args[2] + "\n");
    				fw.write("#\n# Standard BUFR Table " + tbt + ", Master Table " + args[1] + ", Version " + args[2] + "\n");
    			}
    			else {
    				fw.write("Table " + ( tabletype == 4 ? "F" : tbt ) + " LOC |  " + args[1] + " | " + args[5] + " |  " + args[2] + "\n");
    				fw.write("#\n# Local BUFR Table " + tbt + ", Master Table " + args[1] + ", Originating Center " + args[5]
    				         + ", Version " + args[2] + "\n");
    			}
	    		fw.write("# ");
	    	}
	    	else if ( format == 2 ) {  // melbufr
	    		if ( tabletype <= 3 ) {  // Table A, B or D
	    			fw.write("#\n# BUFR Table " + tbt + ", Master Table " + args[1] + ", Version " + args[2] + "\n");
	    		}
	    		else {  // CodeFlag
	    			fw.write("##############################################################################\n");
	    			fw.write("#\n# BUFR Table 0\n#\n# Notes\n# -----\n#\n");
	    			fw.write("# * This table comes from GRIB Code Table 0 (Identification of center)\n#\n");
	    			fw.write("# * A value of 1 in the first column indicates a center that uses minor\n" +
	    					 "#   version numbers with local tables.  Since minor local table version\n" +
	    					 "#   numbers are stored in Section 1, Octet 18, the value in the first\n" +
	    					 "#   column must not be set if a center uses Section 1, Octet 18 for other\n" +
	    					 "#   purposes (i.e., ECMWF).\n#\n");
	    			fw.write("##############################################################################\n\n");
	    			fw.write("# ");
	    		}
	    	}
	    	else if ( format == 3 ) {  // html
	    		fw.write("<html>\n");
	    		fw.write("<head>\n");
	    		fw.write("  <title>BUFR TABLE " + tbt + " - WMO AND LOCAL (NCEP) DESCRIPTORS</title>\n");
	    		fw.write("  <meta name=\"author\" content=\"Jeff Ator\">\n");
	    		fw.write("  <meta http-equiv=Content-Type content=\"text/html\" charset=\"UTF-8\">\n");
	    		fw.write("  <link href=\"BUFRtables_styles.css\" type=\"text/css\" rel=\"stylesheet\">\n");
	    		fw.write("</head>\n");
	    		fw.write("<body>\n");
	    		fw.write("  <h2>BUFR TABLE " + tbt + " - ");
	    		String tLink_TA = "&nbsp;&nbsp;<a href=\"" + html_tbfn.replaceFirst("[A-Za-z]*_", "TableA_") + "\">A</a>";
	    		String tLink_TB = "&nbsp;&nbsp;<a href=\"" + html_tbfn.replaceFirst("[A-Za-z]*_", "TableB_") + "\">B</a>";
	    		String tLink_TD = "&nbsp;&nbsp;<a href=\"" + html_tbfn.replaceFirst("[A-Za-z]*_", "TableD_") + "\">D</a>";
	    		String tLink_CF = "&nbsp;&nbsp;<a href=\"" + html_tbfn.replaceFirst("[A-Za-z]*_", "CodeFlag_") + "\">CODE/FLAG</a>";
	    		String tLinkString = "  <h3>LINKS TO OTHER VERSION " + args[2] + " TABLES:";
	    		if ( tabletype == 1 ) {
	    			fw.write("DATA TYPES AND SUBTYPES");
	    			tLinkString += tLink_TB + tLink_TD + tLink_CF;
	    		}
	    		else if ( tabletype == 2 ) {
	    			fw.write("CLASSIFICATION AND DEFINITION OF DATA ELEMENTS");
	    			tLinkString += tLink_TA + tLink_TD + tLink_CF;
	    		}
	    		else if ( tabletype == 3 ) {
	    			fw.write("COMMON DATA SEQUENCES");
	    			tLinkString += tLink_TA + tLink_TB + tLink_CF;
	    		}
	    		else {
	    			fw.write("ASSOCIATED WITH TABLE B AND");
	    			tLinkString += tLink_TA + tLink_TB + tLink_TD;
	    		}
	    		fw.write(" USED IN THE NCEP OBSERVATIONAL DATABASE AND PREPBUFR AND PREPFITS FILES</h2>\n");
	    		fw.write("  <h2>BASED ON BUFR MASTER TABLE 0 (WMO), VERSION " + args[2]);
	    		String ver;
	    		String vLinkString ="LINKS TO OTHER " + ( tabletype < 4 ?  "TABLE " + tbt : "CODE/FLAG TABLE" ) + " VERSIONS:";
	    		String html_nver;
	    		querystg = "SELECT versionnumber, implementationdate FROM versions WHERE mastertable = 0" +
	    				   "  AND originatingcenter IS NULL ORDER BY versionnumber";
    			queryDB = conn.prepareStatement(querystg);
    			rs_queryDB = queryDB.executeQuery();
    			while ( rs_queryDB.next() ) {
    				ver = rs_queryDB.getString("versionnumber");
    				if ( ( ver.equals(args[2]) ) ) {
    					DateFormat df_old = new SimpleDateFormat("yyyy-MM-dd");
    					DateFormat df_new = new SimpleDateFormat("MMMMMMMMM dd, yyyy");
    					Date iDate = df_old.parse(rs_queryDB.getString("implementationdate"));
    					fw.write(" IMPLEMENTED ON " + df_new.format(iDate));
    					vLinkString += "&nbsp;" + ver;
    				}
    				else {
    					html_nver = html_tbfn.replace("v"+args[2], "v"+ver);
    					vLinkString += "&nbsp;<a href=\"" + html_nver + "\">" + ver + "</a>";
    				}
    			}
	    		fw.write("  </h2>\n  <br />\n");
	    		fw.write(tLinkString + "</h3>\n");
	    		fw.write("  <h3>" + vLinkString + "</h3>\n");
	    	}
	    	fw.write( ( format == 3 ? "<br />\n  <h5>(This webpage" : "This file" ) +
	    			" was generated: " + dateFormat.format(Calendar.getInstance().getTime()) +
	    			( format == 3 ? ")</h5>\n" + ( tabletype < 4 ? "  <br />\n  <hr />\n" : "" ) : "\n#\n") );
	    	
	    	// Write data to output file.
	    	int newX, lastX = -1;
	    	int newY, lastY = -1;
	    	switch ( tabletype ) {
	    	case 1:  // BUFR Table A
	    		if ( format == 2 ) {  // melbufr
	    			for ( int ii = 0; ii < 255; ii++ ) {
		    			querystg = "SELECT DISTINCT meaning FROM bufr_a WHERE version_idx = ? AND code = ?";
		    			queryDB = conn.prepareStatement(querystg);
		    			queryDB.setInt(1, idf.getVersionID());
		    			queryDB.setInt(2, ii);
		    			rs_queryDB = queryDB.executeQuery();
		    			if ( rs_queryDB.next() ) {
		    				fw.write(String.format("%3d\t%s\n", ii, rs_queryDB.getString("meaning")));
		    			}
		    			else {
		    				fw.write(String.format("%3d\tRESERVED\n", ii));
		    			}
	    			}
	    			fw.write("255\tIndicator for local use, with sub-category\n");
	    		}
	    		else if ( format == 3 ) {  // html
	    			fw.write("<p class=\"indent\">\n  <span class=\"bold underline\">IMPORTANT NOTE CONCERNING BUFR TABLE A:</span><br /><br />\n" +
	    					 "WMO standard data subtypes were introduced in BUFR edition 4 to provide a mechanism for better " +
	    					 "understanding of the overall nature and content of BUFR messages exchanged between NWP centers.  In contrast, local " +
	    					 "data subtypes have existed since the advent of BUFR (i.e. since edition 0) and remain available in editions 4+ for " +
	    					 "backwards-compatibility, since many NWP centers including NCEP have made extensive use of them in the past.<br /><br />\n" +
	    					 "What is important to understand is that both values (i.e. the standard subtype and the local subtype) are intended by WMO " +
	    					 "to be supplementary to one another, and therefore both may be present in any given BUFR message when using " +
	    					 "BUFR edition 4 or later.\n<br /><br />\nAll local types and subtypes are shown in italics below.</p><br />\n<hr />\n");
	    			int type;
	    			querystg = "SELECT DISTINCT code, meaning FROM bufr_a WHERE version_idx IN ( 4, ? ) ORDER BY code, subcat_code";
	    			queryDB = conn.prepareStatement(querystg);
	    			queryDB.setInt(1, idf.getVersionID());
	    			rs_queryDB = queryDB.executeQuery();
	    			fw.write("<h3>Data types</h3>\n");
	    			fw.write("<h5 class=\"local\">(local types in italics)</h5>\n");
	    			fw.write("<table rules=groups>\n");
	    			fw.write(" <thead>\n");
	    			fw.write("  <tr>\n");
					fw.write("    <th>Code figure</th>\n");
					fw.write("    <th class=\"left\">Meaning</th>\n");
					fw.write("  </tr>\n");
					fw.write(" </thead>\n");
					fw.write(" <tbody>\n");
					while ( rs_queryDB.next() ) {  // write out quick-links for each of the Table A types
						type = rs_queryDB.getInt("code");
						fw.write(" <tr" + ( ( type <= 101 ) || ( type == 255 ) ? "" : " class=\"local\"" ) + ">\n");
	    				fw.write(String.format("  <td class=\"center\"><a href=\"#type%d\">%d</a></td>\n", type, type));
	    				fw.write(String.format("  <td><a href=\"#type%d\">%s</a></td>\n", type, rs_queryDB.getString("meaning") ));
	    				fw.write(" </tr>\n");
	    			}
	    			fw.write(" </tbody>\n");
	    			fw.write("</table>\n");
	    			rs_queryDB.beforeFirst(); // reset everything to step through the query result set again from the beginning
	    			while ( rs_queryDB.next() ) {
	    				type = rs_queryDB.getInt("code");
	    				// start a new table for this type
	    				fw.write("<br />\n");
	    				fw.write(String.format("<hr id=\"type%d\"/>\n", type));
    					fw.write(String.format("<h3" + ( ( type <= 101 ) || ( type == 255 ) ? "" : " class=\"local\"" ) +
    							">DATA TYPE %d - %s</h3>\n", type, rs_queryDB.getString("meaning")));
    					fw.write("<table style=\"width:100%\"rules=groups>\n");
    					fw.write(" <thead>\n");
    					fw.write("  <tr>\n");
    					fw.write("    <th style=\"width:50%\">Standard subtypes</th>\n");
    					fw.write("    <th class=\"local\" style=\"width:50%\">Local subtypes</th>\n");
    					fw.write("  </tr>\n");
    					fw.write(" </thead>\n");
    					fw.write(" <tbody>\n");
    					fw.write("  <tr>\n");
 
	    				// get the standard subtypes for this type
	    				querystg2 = "SELECT subcat_code, subcat_meaning, code FROM bufr_a WHERE version_idx = ? AND code = ? ORDER BY subcat_code";
	    				queryDB2 = conn.prepareStatement(querystg2);
	    				queryDB2.setInt(1, idf.getVersionID());
	    				queryDB2.setInt(2, type);
	    				rs_queryDB2 = queryDB2.executeQuery();
	   					fw.write("    <td style=\"vertical-align:top\">\n");
    					fw.write("     <table>\n");
    					fw.write("      <thead>\n");
    					fw.write("       <tr>\n");
    					fw.write("        <th style=\"width:10%\">Code</th>\n");
    					fw.write("        <th style=\"width:90%\">Meaning</th>\n");
    					fw.write("       </tr>\n");
    					fw.write("      </thead>\n");
    					fw.write("      <tbody>\n");
	    				if ( ! rs_queryDB2.isBeforeFirst() ) {  // there are no standard subtypes
	    					fw.write("        <tr>\n");
	    					fw.write("         <td></td>\n");
	    					fw.write("         <td></td>\n");
	    					fw.write("        <tr>\n");
	    				}
	    				else {
	    					while ( rs_queryDB2.next() ) {
	    						fw.write("        <tr>\n");
	    						if ( rs_queryDB2.getString("subcat_meaning") == null ) {  // check for null subcat_meaning
	    							fw.write("         <td></td>\n");
	    	    					fw.write("         <td></td>\n");
	    						}
	    						else {
	    							fw.write(String.format("        <td class=\"center\">%d", rs_queryDB2.getInt("subcat_code")) + "</td>\n");
		    						fw.write(String.format("        <td>%s", rs_queryDB2.getString("subcat_meaning")) + "</td>\n");
	    						}
	    						fw.write("        </tr>\n");
	    					}
	    				}
	    				fw.write("      </tbody>\n");
	    				fw.write("     </table>\n");
	    				fw.write("    </td>\n");
	    				// get the local subtypes for this type
	    				querystg3 = "SELECT subcat_code, subcat_meaning, code FROM bufr_a WHERE version_idx = 4 AND code = ? ORDER BY subcat_code";
	    				queryDB3 = conn.prepareStatement(querystg3);
	    				queryDB3.setInt(1, type);
	    				rs_queryDB3 = queryDB3.executeQuery();
	    				fw.write("    <td style=\"vertical-align:top\">\n");
    					fw.write("     <table>\n");
    					fw.write("      <thead>\n");
    					fw.write("       <tr>\n");
    					fw.write("        <th style=\"width:10%\" class=\"local\">Code</th>\n");
    					fw.write("        <th style=\"width:90%\" class=\"local\">Meaning</th>\n");
    					fw.write("       </tr>\n");
    					fw.write("      </thead>\n");
    					fw.write("      <tbody>\n");
	    				if ( ! rs_queryDB3.isBeforeFirst() ) {  // there are no standard subtypes
	    					fw.write("        <tr>\n");
	    					fw.write("         <td></td>\n");
	    					fw.write("        <td></td>\n");
	    					fw.write("        <tr>\n");
	    				}
	    				else {
	    					while ( rs_queryDB3.next() ) {
	    						fw.write("        <tr>\n");
	    						if ( rs_queryDB3.getString("subcat_meaning") == null ) {  // check for null subcat_meaning
	    							fw.write("         <td></td>\n");
	    	    					fw.write("         <td></td>\n");
	    						}
	    						else {
	    							fw.write(String.format("        <td class=\"local center\">%d", rs_queryDB3.getInt("subcat_code")) + "</td>\n");
	    							fw.write(String.format("        <td class=\"local\">%s", rs_queryDB3.getString("subcat_meaning")) + "</td>\n");
	    						}
	    						fw.write("        </tr>\n");
	    					}
	    				}
	    				fw.write("      </tbody>\n");
	    				fw.write("     </table>\n");
	    				fw.write("    </td>\n");
	    				fw.write("  </tr>\n");
	    				fw.write(" </tbody>\n");
	    				fw.write(" </table>\n");
	    				fw.write("<br />\n<div class=\"center\"><a href=\"#\">Back to Top</a></div>\n");
	    			}
	    			fw.write("</body>\n");
	    			fw.write("</html>\n");
	    		}
	    		break;
	    	case 2:  // BUFR Table B
	    		querystg = "SELECT fxy, class, class_description, element_name, bufr_unit, " +
	    		           "  bufr_scale, bufr_reference_value, bufr_data_width, mnemonic FROM bufr_b " +
	    		           "WHERE version_idx";
	    		if ( format == 3 ) {
	    			querystg += " IN ( 4, ? ) ORDER BY fxy";  // include the NCEP local descriptors (which are version_idx = 4) in the search
	    		}
	    		else {
	    			querystg += " = ? AND mnemonic IS NOT NULL ORDER BY fxy";
	    		}
	    		queryDB = conn.prepareStatement(querystg);
	    		queryDB.setInt(1, idf.getVersionID());
	    		rs_queryDB = queryDB.executeQuery();
	    		switch ( format ) {
	    		case 1:  // bufrlib
	    			fw.write("#========================================================================================================\n");
	    			fw.write("# F-XX-YYY |SCALE| REFERENCE   | BIT |      UNIT         | MNEMONIC ;DESC ;  ELEMENT NAME\n");
	    			fw.write("#          |     |   VALUE     |WIDTH|                   |          ;CODE ;\n");
	    			fw.write("#========================================================================================================\n");
	    			while ( rs_queryDB.next() ) {
	    				outline = String.format("  %s-%s-%s |%4d | %11d | %3d | %-17s | %-8s ;     ; %s\n",
	    					da.getF(rs_queryDB.getString("fxy")), da.getX(rs_queryDB.getString("fxy")),
	    					da.getY(rs_queryDB.getString("fxy")), rs_queryDB.getInt("bufr_scale"),
	    					rs_queryDB.getInt("bufr_reference_value"), rs_queryDB.getInt("bufr_data_width"),
		    				rs_queryDB.getString("bufr_unit"), rs_queryDB.getString("mnemonic"),
		    				rs_queryDB.getString("element_name") );
	    				fw.write(outline);
		    		}
	    			fw.write("END\n");
	    			break;
	    		case 2:  // melbufr
	    			fw.write("#                   Reference    Bit#\n");
	    			fw.write("# F   X    Y Scale    Value     Width     Units     Comments\n");
	    			fw.write("#------------------------------------------------------------\n");
	    			while ( rs_queryDB.next() ) {
	    				newX = Integer.parseInt(da.getX(rs_queryDB.getString("fxy")));
	    				if ( newX > lastX ) {
	    					fw.write(String.format("#Class %02d - %s\n", newX, rs_queryDB.getString("class")));
	    					lastX = newX;
	    				}
	    				outline = String.format("%d; %d; %d; %d; %d; %d; %s; %s\n",
	    					Integer.parseInt(da.getF(rs_queryDB.getString("fxy"))), newX,
		    				Integer.parseInt(da.getY(rs_queryDB.getString("fxy"))), rs_queryDB.getInt("bufr_scale"),
		    				rs_queryDB.getInt("bufr_reference_value"), rs_queryDB.getInt("bufr_data_width"),
			    			rs_queryDB.getString("bufr_unit"), rs_queryDB.getString("element_name") );
	    				fw.write(outline);
	    			}
	    			break;
	    		default:  // html
	    			String f, x, y, cd;
	    			String html_cffn = html_tbfn.replace("TableB", "CodeFlag");
	    			fw.write("<h3>Classification of elements</h3>\n");
	    			fw.write("<h5 class=\"local\">(local classes in italics)</h5>\n");
	    			fw.write("<table rules=groups>\n");
	    			fw.write(" <thead>\n");
	    			fw.write("  <tr>\n");
					fw.write("    <th>F</th>\n");
					fw.write("    <th>X</th>\n");
					fw.write("    <th class=\"left\">Class</th>\n");
					fw.write("    <th class=\"left\">Description</th>\n");
					fw.write("  </tr>\n");
					fw.write(" </thead>\n");
					fw.write(" <tbody>\n");
	    			while ( rs_queryDB.next() ) {  // write out quick-links for each of the Table B classes
	    				x = da.getX(rs_queryDB.getString("fxy"));
	    				newX = Integer.parseInt(x);
	    				if ( newX > lastX ) {
	    					fw.write(" <tr" + ( da.isLocal(newX,0) ? " class=\"local\"" : "" ) + ">\n");
	    					fw.write("  <td>0</td>\n");
	    					fw.write(String.format("  <td><a href=\"#class%02d\">%02d</a></td>\n", newX, newX));
	    					fw.write(String.format("  <td><a href=\"#class%02d\">%s</a></td>\n", newX, rs_queryDB.getString("class") ));
	    					if ( ( cd = rs_queryDB.getString("class_description") ) == null ) cd = " ";  // check for null class_description
	    					fw.write(String.format("  <td>%s</td>\n", cd ));
	    					fw.write(" </tr>\n");
	    					lastX = newX;
	    				}
	    			}
	    			fw.write(" </tbody>\n");
	    			fw.write("</table>\n");
	    			rs_queryDB.beforeFirst(); lastX = -1; // reset everything to step through the query result set again from the beginning
	    			boolean printTable = true;  // most classes have a table of individual elements, but a few do not
	    			while ( rs_queryDB.next() ) {  // write out all of the individual elements in each Table B class
	    				f = da.getF(rs_queryDB.getString("fxy"));
	    				x = da.getX(rs_queryDB.getString("fxy"));
	    				y = da.getY(rs_queryDB.getString("fxy"));
	    				newX = Integer.parseInt(x);
	    				if ( newX > lastX ) {
	    					if ( ( lastX != -1 ) && ( printTable ) ) fw.write(" </tbody>\n</table>\n<br />\n" +
	    							"<div class=\"center\"><a href=\"#\">Back to Top</a></div>\n<br />\n");
	    					fw.write("<br />\n");
	    					fw.write(String.format("<hr id=\"class%02d\">\n", newX));
	    					fw.write("<h3" + ( da.isLocal(newX,0) ? " class=\"local\"" : "" ) + ">\n");
	    					fw.write(String.format("CLASS %02d - %s</h3>\n", newX, rs_queryDB.getString("class")));
	    					fw.write("<h5 class=\"local\">(local elements in italics)</h5>\n");
	    					printTable = ( ( ( newX == 54 ) || ( newX == 60 ) ) ? false : true );
	    					if ( printTable ) {
	    						fw.write("<table rules=groups>\n");
		    					fw.write(" <thead>\n");
		    					fw.write("  <tr>\n");
		    					fw.write("    <th style=\"width:10%\">DESCRIPTOR</th>\n");
		    					fw.write("    <th style=\"width:40%\">ELEMENT NAME</th>\n");
		    					fw.write("    <th style=\"width:15%\">UNIT</th>\n");
		    					fw.write("    <th style=\"width:5%\">SCALE</th>\n");
		    					fw.write("    <th style=\"width:12%\">REFERENCE VALUE</th>\n");
		    					fw.write("    <th style=\"width:8%\">DATA WIDTH</th>\n");
		    					fw.write("    <th style=\"width:10%\">MNEMONIC</th>\n");
		    					fw.write("  </tr>\n");
		    					fw.write(" </thead>\n");
		    					fw.write(" <tbody>\n");
	    					}
	    					lastX = newX;
	    				}
	    				if ( printTable ) { 
	    					fw.write("  <tr" + ( da.isLocal(newX,Integer.parseInt(y)) ? " class=\"local\"" : "" ) + ">\n");
	    					fw.write("    <td>" + String.format("%s-%s-%s", f, x, y) + "</td>\n");
		    				fw.write("    <td>" + rs_queryDB.getString("element_name") + "</td>\n");
		    				fw.write("    <td>");
		    				if ( rs_queryDB.getString("bufr_unit").contains("table") ) {  // insert hyperlink to code or flag table
		    					fw.write("<a href=\"" + html_cffn + String.format("#%s%s%s",f,x,y) + "\">" +
		    						rs_queryDB.getString("bufr_unit") + "</a>");
		    				}
		    				else {
		    					fw.write(rs_queryDB.getString("bufr_unit"));
		    				}
		    				fw.write("</td>\n");
		    				fw.write("    <td class=\"right\">" + String.format("%d", rs_queryDB.getInt("bufr_scale")) + "</td>\n");
		    				fw.write("    <td class=\"right\">" + String.format("%d", rs_queryDB.getInt("bufr_reference_value")) + "</td>\n");
		    				fw.write("    <td class=\"right\">" + String.format("%d", rs_queryDB.getInt("bufr_data_width")) + "</td>\n");
		    				fw.write("    <td>" + rs_queryDB.getString("mnemonic") + "</td>\n");
		    				fw.write("  </tr>\n");
	    				}
	    			}
	    			if ( printTable ) fw.write("</tbody>\n</table>\n<br />\n<div class=\"center\"><a href=\"#\">Back to Top</a></div>\n");
	    			fw.write("</body>\n");
	    			fw.write("</html>\n");
	    			break;
	    		}
	    		break;
	    	case 3:  // BUFR Table D
    			String lastfxy = "";
    			String lastreffxy = "";
    			String lastename = "";
    			String csq;
    			String f, x, y, catg;
	    		if ( ! tableSTD ) {  // Local Table D
    				fw.write("# NOTE: Where a local Table D sequence contains a standard Table B or Table D element,\n" +
    						 "#       the definition corresponding to that element is left blank in this file, since\n" +
    						 "#       the definition could vary depending on what version of the standard tables is\n" +
    						 "#       used with that local sequence in a given BUFR message.\n#\n");
    			}
	    		querystg = "SELECT fxy, category, cat_seq, mnemonic, ref_fxy, ref_element_name, " +
	    				   "ref_mnemonic, version_idx, order_idx FROM bufr_d " +
		           		   "WHERE version_idx " + ( ( format == 3 ) ? "IN ( 4, ? )" : "= ?" ) + " ORDER BY fxy, order_idx";;
	    		queryDB = conn.prepareStatement(querystg);
	    		queryDB.setInt(1, idf.getVersionID());
	    		rs_queryDB = queryDB.executeQuery();
	    		if ( format == 1 ) {   // bufrlib
	    			fw.write("#========================================================================================================\n");
	    			fw.write("# F-XX-YYY | MNEMONIC   ;DCOD ; NAME           <-- sequence definition\n");
	    			fw.write("#          | F-XX-YYY > | NAME                 <-- element definition (first thru next-to-last)\n");
	    			fw.write("#          | F-XX-YYY   | NAME                 <-- element definition (last)\n");
	    			fw.write("#========================================================================================================\n\n");
	    		}
	    		else if ( format == 3 ) {  // html
		    		fw.write("<h3>Categories of common sequences</h3>\n");
		    		fw.write("<h5 class=\"local\">(local categories in italics)</h5>\n");
		    		fw.write("<table rules=groups>\n");
		    		fw.write(" <thead>\n");
		    		fw.write("  <tr>\n");
					fw.write("    <th>F</th>\n");
					fw.write("    <th>X</th>\n");
					fw.write("    <th class=\"left\">Category</th>\n");
					fw.write("  </tr>\n");
					fw.write(" </thead>\n");
					fw.write(" <tbody>\n");
		    		while ( rs_queryDB.next() ) {  // write out quick-links for each of the Table D categories
		    			x = da.getX(rs_queryDB.getString("fxy"));
		    			newX = Integer.parseInt(x);
		    			if ( newX > lastX ) {
		    				fw.write(" <tr" + ( da.isLocal(newX,0) ? " class=\"local\"" : "" ) + ">\n");
		    				fw.write("  <td>3</td>\n");
		    				fw.write(String.format("  <td><a href=\"#category%02d\">%02d</a></td>\n", newX, newX));
		    				if ( ( catg = rs_queryDB.getString("category") ) == null ) catg = "Miscellaneous";
		    				fw.write(String.format("  <td><a href=\"#category%02d\">%s</a></td>\n", newX, catg ));
		    				fw.write(" </tr>\n");
		    				lastX = newX;
		    			}
		    		}
		    		fw.write(" </tbody>\n");
		    		fw.write("</table>\n");
		    		rs_queryDB.beforeFirst(); lastX = -1; // reset everything to step through the query result set again from the beginning
	    		}
	    		boolean printCategory = true;  // most categories contain a list of sequences, but a few do not
	    		boolean printSequence = true;  // for local descriptors, only write out the sequence name and description (do not write out the full sequence)
	    		while ( rs_queryDB.next() ) {  // write out all of the sequences in each Table D category
	    			if ( rs_queryDB.getInt("order_idx") == 1 ) {  // this is the start of a new Table D entry
	    				if ( ! lastfxy.isEmpty() ) {
	    					// write last line of previous Table D entry
	    					switch ( format ) {
	    					case 1:  // bufrlib
		    				    fw.write(String.format("           | %s-%s-%s   | %s\n\n",
			    				    	da.getF(lastreffxy), da.getX(lastreffxy), da.getY(lastreffxy), lastename ));
		    				    break;
	    					case 2:  // melbufr
	    						fw.write(String.format("       %d %2d %3d  %s\n      -1\n",
	    							Integer.parseInt(da.getF(lastreffxy)), Integer.parseInt(da.getX(lastreffxy)),
	    							Integer.parseInt(da.getY(lastreffxy)), lastename ));
	    						break;
	    					default:  // html
	    						if ( lastX != -1 ) {
	    							if ( printSequence ) {
			    						fw.write("  <tr>\n");
				    					fw.write("    <td></td>\n");
				    					fw.write("    <td>" + String.format("%s-%s-%s", da.getF(lastreffxy), da.getX(lastreffxy), da.getY(lastreffxy)) + "</td>\n");
				    					fw.write("    <td>" + lastename + "</td>\n");
				    					fw.write("  </tr>\n");
	    							}
			    					fw.write(" </tbody>\n");
			    		    		fw.write("</table>\n");
	    						}
	    						break;
	    					}
	    				}  
	    				// write first line of new entry
	    				lastfxy = rs_queryDB.getString("fxy");
	    				f = da.getF(lastfxy);
	    				x = da.getX(lastfxy);
	    				y = da.getY(lastfxy);
	    				if ( ( csq = rs_queryDB.getString("cat_seq") ) == null ) csq = "";
	    				switch ( format ) {
	    				case 1:  // bufrlib
		    				fw.write(String.format("  %s-%s-%s | %-8s   ;     ; %s\n", f, x, y, rs_queryDB.getString("mnemonic"), csq ) );
		    				break;
	    				case 2:  // melbufr
	    					newX = Integer.parseInt(x);
	    					if ( newX > lastX ) {
	    						fw.write(String.format("#Category %02d - %s\n", newX, rs_queryDB.getString("category")));
	    						lastX = newX;
	    					}
	    					if ( ! csq.isEmpty() ) fw.write("#  " + csq + "\n" );
	    					fw.write(String.format("%d %2d %3d\n", Integer.parseInt(f), newX, Integer.parseInt(y)));
	    					break;
	    				default:  // html
    			    		newX = Integer.parseInt(x);
    			    		if ( newX > lastX ) {  // write out a header for this new category
    			    			if ( lastX != -1 ) fw.write("<br />\n<div class=\"center\"><a href=\"#\">Back to Top</a></div>\n<br />\n");
    			    			fw.write("<br />\n");
    			    			fw.write(String.format("<hr id=\"category%02d\">\n", newX));
    			    			fw.write("<h3" + ( da.isLocal(newX,0) ? " class=\"local\"" : "" ) + ">");
    			    			if ( ( catg = rs_queryDB.getString("category") ) == null ) catg = "Miscellaneous";
    			    			fw.write(String.format("CATEGORY %02d - %s</h3>\n", newX, catg ));
    			    			fw.write("<h5 class=\"local\">(local sequences in italics)</h5>\n");
    			    			printCategory = ( ( newX == 54 ) ? false : true );
    			    			lastX = newX;
    			    		}
	    					if ( printCategory ) {
	    						printSequence = da.isStandard(newX,Integer.parseInt(y));  
	    						fw.write("<br />\n");
	    						fw.write("<table rules=groups>\n");
		    					fw.write(" <thead>\n");
		    					fw.write("  <tr" + ( printSequence ? "" : " class=\"local\"" ) + ">\n");
		    					fw.write("    <th>" + String.format("%s-%s-%s", f, x, y) + "</th>\n");
		    					fw.write("    <th style=\"width:150\">" + rs_queryDB.getString("mnemonic") + "</th>\n");
		    					fw.write("    <th style=\"width:750; padding-left:25px;\" class=\"left\">" + 
		    								( csq.isEmpty() ? csq : "(" + csq + ")" ) + "</th>\n");
		    					fw.write("  </tr>\n");
		    					fw.write(" </thead>\n");
		    					fw.write(" <tbody>\n");
	    					}
	    					else {
	    						printSequence = false;  // if we're not writing the category, then we obviously don't want/need to write any of its sequences
	    					}
	    					break;
	    				}
	    			}	
	    			else if ( lastreffxy != null ){
	    				// write previous line of current Table D entry
	    				f = da.getF(lastreffxy);
	    				x = da.getX(lastreffxy);
	    				y = da.getY(lastreffxy);
	    				switch ( format ) {
	    				case 1:  // bufrlib
	    					fw.write(String.format("           | %s-%s-%s > | %s\n", f, x, y, lastename ));
	    					break;
	    				case 2:  // melbufr
	    					fw.write(String.format("       %d %2d %3d  %s\n",
	    						Integer.parseInt(f), Integer.parseInt(x), Integer.parseInt(y), lastename ));
	    					break;
	    				default: // html
	    					if ( printSequence ) {
	    						fw.write("  <tr>\n");
		    					fw.write("    <td></td>\n");
		    					fw.write("    <td>" + String.format("%s-%s-%s", f, x, y) + "</td>\n");
		    					fw.write("    <td>" + lastename + "</td>\n");
		    					fw.write("  </tr>\n");
	    					}
	    					break;
	    				}
	    			}
		    		lastreffxy = rs_queryDB.getString("ref_fxy");
		    		if ( ( lastename = rs_queryDB.getString("ref_element_name") ) == null ) lastename = "";
	    		}
		    	// write last line of last Table D entry
		    	if ( format == 1 ) {
					outline = String.format("           | %s-%s-%s   | %s\n", 
							da.getF(lastreffxy), da.getX(lastreffxy), da.getY(lastreffxy), lastename );
					fw.write(outline);
		    		fw.write("END\n");
		    	}
		    	else if ( format == 3 ) {  // html
					if ( printSequence ) {
						fw.write("  <tr>\n");
    					fw.write("    <td></td>\n");
    					fw.write("    <td>" + String.format("%s-%s-%s", da.getF(lastreffxy), da.getX(lastreffxy), da.getY(lastreffxy)) + "</td>\n");
    					fw.write("    <td>" + lastename + "</td>\n");
    					fw.write("  </tr>\n");
					}
					fw.write(" </tbody>\n");
		    		fw.write("</table>\n");
		    		fw.write("<br />\n<div class=\"center\"><a href=\"#\">Back to Top</a></div>\n<br />\n");
		    		fw.write("</body>\n");
	    			fw.write("</html>\n");
		    	}
	    		break;
	    	case 4:  // CodeFlag
	    		if ( format == 2 ) {  // melbufr
		    		querystg = "SELECT value, meaning FROM code_flag_lookup WHERE bufr_b_idx = " +
		    				"(SELECT id FROM bufr_b WHERE fxy = '001033' AND version_idx = ?) ORDER BY value";
		    		queryDB = conn.prepareStatement(querystg);
		    		queryDB.setInt(1, idf.getVersionID());
		    		rs_queryDB = queryDB.executeQuery();
	    			while ( rs_queryDB.next() ) {
	    				fw.write(String.format("0\t%d\t%s\n", rs_queryDB.getInt("value"), rs_queryDB.getString("meaning")));
	    			}
	    			fw.write("0\t255\tMissing value\n");
	    		}
	    		else if ( ( format == 1 ) || ( format == 3 ) ) {  // bufrlib or html
	    			querystg = "SELECT b.fxy, b.element_name, b.mnemonic, b.bufr_data_width, cf.value, cf.bitnumber, cf.meaning, cf.depends_on " +
	    				"FROM code_flag_lookup cf JOIN bufr_b b ON b.id = cf.bufr_b_idx WHERE b.version_idx ";
	    			if ( format == 1 ) {  // for bufrlib, we have separate STD and LOC files
	    				querystg += "= " + ( tableSTD ? String.format("%d", idf.getVersionID() ) : "4" );
	    				fw.write("#========================================================================================================\n");
	    				fw.write("# F-XX-YYY | MNEMONIC ; CODE               <-- code table definition\n");
	    				fw.write("#          | F-XX-YYY=VAL                  <-- dependencies, if any (comma-separated if more than one)\n");
	    				fw.write("#            | VAL > | MEANING               <-- code figure and meaning (first thru next-to-last)\n");
	    				fw.write("#            | VAL   | MEANING               <-- code figure and meaning (last)\n");
	    				fw.write("#========================================================================================================\n");
	    				fw.write("# F-XX-YYY | MNEMONIC ; FLAG               <-- flag table definition\n");
	    				fw.write("#          | F-XX-YYY=BIT                  <-- dependencies, if any (comma-separated if more than one)\n");
	    				fw.write("#            | BIT > | MEANING               <-- bit number and meaning (first thru next-to-last)\n");
	    				fw.write("#            | BIT   | MEANING               <-- bit number and meaning (last)\n");
	    				fw.write("#========================================================================================================\n");
	    			}
	    			else {  // for html, both standard and local entries go into the same file
	    				querystg += "IN ( 4, " + String.format("%d", idf.getVersionID() ) + " )";
	    			}
	    			querystg += " ORDER BY b.fxy, cf.depends_on, cf.value, cf.bitnumber";
	    			queryDB = conn.prepareStatement(querystg);
		    		rs_queryDB = queryDB.executeQuery();
		    		querystg2 = "SELECT b.fxy, b.element_name, b.mnemonic, cf.id, cf.value, cf.meaning " +
    					"FROM code_flag_lookup cf JOIN bufr_b b ON b.id = cf.bufr_b_idx " +
		    			"WHERE cf.id = ?";
		    		lastX = -1;
		    		lastY = -1;
		    		/* Declare the following variables as int rather than Integer, because == and != comparisons
		    		   are unreliable for Integer values outside the approximate range of [-128,127] */
		    		int newValue, lastValue = -1;
		    		int newDependsOn, lastDependsOn = 0;
		    		int newDX, newDY;
		    		int bdw = 0;
		    		String DX, DY;
		    		String lastMeaning = "";
		    		/*  For 0-02-195, 0-20-105 and each of the PREPBUFR quality markers, consolidate all of the code figures and meanings which are shared by
		    		 * multiple "depends_on" values into a single bufrlib entry or HTML table */
		    		tableConsolidator tC_02_195 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(130,131,134,135,230,231,234,235) ),
		    				new ArrayList<Integer>( Arrays.asList(151,156,157,158,159,164,165,174,175) ),
		    				new ArrayList<Integer>( Arrays.asList(120,122,132,220,221,222,232) ),
		    				new ArrayList<Integer>( Arrays.asList(241,242,243,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259) ) );
		    		tableConsolidator tC_07_246 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(5,6,7,8,9,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ),
		    				new ArrayList<Integer>( Arrays.asList(2) ), new ArrayList<Integer>( Arrays.asList(4) ), new ArrayList<Integer>( Arrays.asList(10) ));
		    		tableConsolidator tC_10_246 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(2,5,6,7,8,9,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ),
		    				new ArrayList<Integer>( Arrays.asList(4) ), new ArrayList<Integer>( Arrays.asList(10) ));
		    		tableConsolidator tC_11_217 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(5,6,8,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ),
		    				new ArrayList<Integer>( Arrays.asList(2) ), new ArrayList<Integer>( Arrays.asList(4) ), new ArrayList<Integer>( Arrays.asList(10) ),
		    				new ArrayList<Integer>( Arrays.asList(7) ), new ArrayList<Integer>( Arrays.asList(9) ));
		    		tableConsolidator tC_11_218 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(5,6,8,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ),
		    				new ArrayList<Integer>( Arrays.asList(2) ), new ArrayList<Integer>( Arrays.asList(4) ), new ArrayList<Integer>( Arrays.asList(10) ),
		    				new ArrayList<Integer>( Arrays.asList(7) ), new ArrayList<Integer>( Arrays.asList(9) ));
		    		tableConsolidator tC_11_240 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(5,6,8,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ),
		    				new ArrayList<Integer>( Arrays.asList(2) ), new ArrayList<Integer>( Arrays.asList(4) ), new ArrayList<Integer>( Arrays.asList(10) ),
		    				new ArrayList<Integer>( Arrays.asList(7) ), new ArrayList<Integer>( Arrays.asList(9) ));
		    		tableConsolidator tC_12_246 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(2,5,6,9,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ),
		    				new ArrayList<Integer>( Arrays.asList(4) ), new ArrayList<Integer>( Arrays.asList(7) ), new ArrayList<Integer>( Arrays.asList(8) ),
		    				new ArrayList<Integer>( Arrays.asList(10) ));
		    		tableConsolidator tC_13_246 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(2,5,6,7,8,9,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ),
		    				new ArrayList<Integer>( Arrays.asList(4) ), new ArrayList<Integer>( Arrays.asList(10) ));
		    		tableConsolidator tC_20_105 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(1,2,3,4,5,6,7,8,9) ), new ArrayList<Integer>( Arrays.asList(0) ) );
		    		tableConsolidator tC_20_246 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(2,4,5,6,7,8,9,10,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ));
		    		tableConsolidator tC_22_246 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(2,4,5,6,7,8,9,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ),
		    				new ArrayList<Integer>( Arrays.asList(10) ));
		    		tableConsolidator tC_51_001 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(2,4,5,6,7,8,9,10,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ));
		    		tableConsolidator tC_51_021 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(2,5,6,7,8,9,11,12,13,14) ), new ArrayList<Integer>( Arrays.asList(1) ),
		    				new ArrayList<Integer>( Arrays.asList(4) ), new ArrayList<Integer>( Arrays.asList(10) ));
		    		tableConsolidator tC_55_009 = new tableConsolidator(
		    				new ArrayList<Integer>( Arrays.asList(130,230) ), new ArrayList<Integer>( Arrays.asList(131,135,231,235) ),
		    				new ArrayList<Integer>( Arrays.asList(132,232) ));
		    		boolean isCodeTable = false;
		    		boolean printTable = true;  // most tables will be printed, but a few are duplicates of others and will be skipped
		    		boolean changedFXY, changedDependsOn;
		    		while ( rs_queryDB.next() ) {  // cycle through each entry in the result set
		    			f = da.getF(rs_queryDB.getString("b.fxy"));
	    				x = da.getX(rs_queryDB.getString("b.fxy"));
	    				y = da.getY(rs_queryDB.getString("b.fxy"));
	    				newX = Integer.parseInt(x);
	    				newY = Integer.parseInt(y);
	    				changedFXY = ( ( newX != lastX ) || ( newY != lastY ) );
	    				newDependsOn = rs_queryDB.getInt("cf.depends_on");  // will automatically set to 0 if corresponding result set entry is NULL
	    				changedDependsOn = ( newDependsOn != lastDependsOn );
	    				if ( ( changedFXY ) || ( changedDependsOn ) ) {  // start a new code or flag table for this entry
	    					if ( ( lastX != -1 ) && ( printTable ) ) {  // but first close out any previous code or flag table
	    						if ( format == 1 ) {
	    							fw.write( String.format("              | %d %s | ", lastValue, ( changedFXY ? " " : ">") ) + lastMeaning + "\n" );
	    						}
	    						else {
	    							closeTable( fw, lastX, lastY, bdw, lastValue, isCodeTable);  
	    						}
	    					}
	    					bdw = rs_queryDB.getInt("b.bufr_data_width");
	    					isCodeTable = ( rs_queryDB.getString("cf.bitnumber") == null );
	    					printTable = true;
	    					if ( changedFXY ) {
	    						if ( format == 1 ) {
	    							fw.write(String.format("\n  %s-%s-%s | %s ; ", f, x, y, rs_queryDB.getString("b.mnemonic") ));
	    							fw.write( ( isCodeTable ? "CODE" : "FLAG") + "\n");
	    						}
	    						else {
	    							fw.write("<br />\n");
	    							fw.write(String.format("<hr id=\"%s%s%s\">\n", f, x, y));
			    					fw.write(String.format("<h4>%s-%s-%s - %s</h4>\n", f, x, y, rs_queryDB.getString("b.mnemonic")));
			    	    			fw.write("<h4>" + rs_queryDB.getString("b.element_name") + "</h4>\n");
	    						}
	    					}
	    					if ( ( changedDependsOn ) && ( newDependsOn != 0 ) ) {
	    						queryDB2 = conn.prepareStatement(querystg2);
	    						queryDB2.setInt(1, newDependsOn );
	    						rs_queryDB2 = queryDB2.executeQuery();
	    						rs_queryDB2.next();  // there should be exactly one entry in the result set
	    						DX = da.getX(rs_queryDB2.getString("b.fxy"));
	    						DY = da.getY(rs_queryDB2.getString("b.fxy"));
	    						newDX = Integer.parseInt(DX);
	    						newDY = Integer.parseInt(DY);
	    						if ( ( newX == 1 ) && ( ( newY == 32 ) || ( newY == 34 ) ) && 
	    							( newDX == 1 ) && ( newDY == 35 ) ) {
	    							if ( format == 3 ) {
	    								fw.write("<br />\n");
		    							fw.write(String.format("<p class=\"left\">When the value of<br>\n" +
		    								"<span class=\"bold indent\">0-01-031 - GCLONG (Identification of originating/generating centre)</span><br>\n" +
		    								"<span class=\"indentX2\">or</span><br>\n" +
		    								"<span class=\"bold indent\">0-01-033 - OGCE (Identification of originating/generating centre)</span><br>\n" +
		    								"<span class=\"indentX2\">or</span><br>\n" +
		    								"<span class=\"bold indent\">0-01-035 - ORIGC (Originating centre)<br><br>\n= %d (%s)</span></p>\n",
		    								rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning")) );
	    							}
	    							else if ( tableSTD ) {
	    								fw.write(String.format("           | 0-01-031,0-01-033,0-01-035=%d\n", rs_queryDB2.getInt("cf.value")));
	    							}
	    						}
	    						else if ( ( newX == 1 ) && ( ( newY == 32 ) || ( newY == 34 ) ) && 
		    							( newDX == 1 ) && ( ( newDY == 31 ) || ( newDY == 33 ) ) ) {
		    						printTable = false;	
		    					}
	    						else if ( ( newX == 2 ) && ( newY == 195 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_02_195.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_02_195.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 7 ) && ( newY == 246 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_07_246.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_07_246.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 10 ) && ( newY == 246 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_10_246.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_10_246.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 11 ) && ( newY == 217 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_11_217.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_11_217.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 11 ) && ( newY == 218 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_11_218.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_11_218.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 11 ) && ( newY == 240 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_11_240.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_11_240.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 12 ) && ( newY == 246 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_12_246.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_12_246.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 13 ) && ( newY == 246 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_13_246.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_13_246.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 20 ) && ( newY == 105 ) && ( ( format == 3 ) || ( tableSTD ) ) ) {  // 0-20-105 is a standard descriptor
	    							if ( printTable = tC_20_105.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_20_105.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 20 ) && ( newY == 246 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_20_246.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_20_246.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 22 ) && ( newY == 246 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_22_246.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_22_246.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 51 ) && ( newY == 1 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_51_001.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_51_001.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 51 ) && ( newY == 21 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_51_021.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_51_021.getConsolidatedTable());
	    						}
	    						else if ( ( newX == 55 ) && ( newY == 9 ) && ( ( format == 3 ) || ( ! tableSTD ) ) ) {
	    							if ( printTable = tC_55_009.processNewValue( format, rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning"),
	    									rs_queryDB2.getString("b.mnemonic"), da.getF(rs_queryDB2.getString("b.fxy")), DX, DY,
	    									rs_queryDB2.getString("b.element_name") ) ) fw.write(tC_55_009.getConsolidatedTable());
	    						}
	    						else {
	    							if ( format == 1 ) {
	    								fw.write(String.format("           | %s-%s-%s=%d\n", da.getF(rs_queryDB2.getString("b.fxy")), DX, DY, rs_queryDB2.getInt("cf.value") ));
	    							}
	    							else {
	    								fw.write("<br />\n");
			    						fw.write(String.format("<p class=\"left\">When the value of<br>\n" +
			    							"<span class=\"bold indent\">%s-%s-%s - %s (%s)<br><br>\n= %d (%s)</span></p>\n",
			    							da.getF(rs_queryDB2.getString("b.fxy")), DX, DY, 
			    							rs_queryDB2.getString("b.mnemonic"), rs_queryDB2.getString("b.element_name"),
			    							rs_queryDB2.getInt("cf.value"), rs_queryDB2.getString("cf.meaning") ) );
	    							}
	    						}
	    					}
	    	    			if ( ( printTable ) && ( format == 3 ) ) {
	    	    				fw.write("<table rules=groups>\n");
	    	    				fw.write(" <thead>\n");
		    	    			fw.write("  <tr>\n");
		    					fw.write("    <th>" + ( isCodeTable ? "Code figure" : "Bit number") + "</th>\n");
		    					fw.write("    <th class=\"left\">Meaning</th>\n");
		    					fw.write("  </tr>\n");
		    					fw.write(" </thead>\n");
		    					fw.write(" <tbody>\n");
	    	    			}
	    					lastX = newX;
	    					lastY = newY;
	    					lastValue = ( isCodeTable ? -1 : 0 );
	    					lastMeaning = "";
	    					lastDependsOn = newDependsOn;
	    				}
	    				newValue = ( isCodeTable ? rs_queryDB.getInt("cf.value") : rs_queryDB.getInt("cf.bitnumber") );
	    				if ( printTable ) { // add this entry to the current code or flag table
	    					if ( format == 1 ) {
	    						if ( ! lastMeaning.isEmpty() ) {
	    							fw.write( String.format("              | %d > | ", lastValue ) + lastMeaning + "\n" );
	    						}
	    						lastMeaning = rs_queryDB.getString("cf.meaning");
	    					}
	    					else {
	    						fillReserved( fw, lastValue, newValue );  // but first check for any skipped values corresponding to "Reserved" entries
		    					fw.write("  <tr>\n");
		    					fw.write("    <td class=\"center\">" + String.format("%d</td>\n", newValue ) );
		    					fw.write("    <td>" + rs_queryDB.getString("cf.meaning") + "</td>\n");
		    					fw.write("  </tr>\n");
	    					}	
	    				}
	    				lastValue = newValue;
		    		}
		    		if ( format == 3 ) {
		    			if ( printTable ) closeTable( fw, lastX, lastY, bdw, lastValue, isCodeTable);  // close out the final code or flag table
			    		fw.write("</body>\n");
		    			fw.write("</html>\n");
		    		}
		    		else {
		    			fw.write( String.format("              | %d   | ", lastValue ) + lastMeaning + "\n" );
		    			fw.write("END\n");
		    		}
	    		}
	    		break;
			default:
				break;	
	    	}
	    	
	    	//  Close the output file.
	    	fw.close();
	    	
	    	//  Close the database.
	    	conn.close();
	    }
	    catch(Exception ex) {
	    	ex.printStackTrace();
	    }
	   
	}
	
	
	//  Method to fill in "Reserved" gaps in the current code or flag table.
	
	public static void fillReserved ( FileWriter fw, int lastval, int newval ) {
		/*
		**	all arguments are input, including:
		**		lastval = last numerical code figure or bit number entry that was written to the current code or flag table
		**		newval = current code figure or bit number entry, to be compared to lastval to determine if there's a gap that needs to be filled
		*/
		try {
			if ( lastval + 1 != newval ) {
				fw.write("  <tr>\n");
				fw.write("    <td class=\"center\">" + ( lastval + 1 != newval - 1 ? String.format("%d-", lastval + 1) : "" ) +
					String.format("%d</td>\n", newval - 1 ) );
				fw.write("    <td>Reserved</td>\n");
				fw.write("  </tr>\n");
			}
		}
		catch(Exception ex) {
			ex.printStackTrace();
		}
	}
	
	
	//  Method to close out the current code or flag table.
	
	public static void closeTable ( FileWriter fw, int x, int y, int bitwidth, int lastval, boolean isCodeTable ) {
		/*
		**	all arguments are input, including:
		**		x = X value of table being closed
		**		y = Y value of table being closed
		**		bitwidth = number of bits occupied by table
		**		lastval = last numerical code figure or bit number entry in the table that was neither "Reserved" nor "Missing"
		**		isCodeTable = true iff the table is a code table, false iff it is a flag table
		*/
		Integer missingValue;
		try {
			if (  ( ( x != 31 ) || ( y != 31 ) ) &&
					( ( x != 55 ) || ( y < 20 ) || ( y > 22 ) )  ) {  // for all tables other than 0-31-031, 0-55-020, 0-55-021 and 0-55-022
				missingValue = ( isCodeTable ? ( 1 << bitwidth ) - 1 : bitwidth );
				fillReserved( fw, lastval, missingValue );  // check for any "Reserved" entries prior to the missingValue
				fw.write("  <tr>\n");
				fw.write("    <td class=\"center\">" + ( isCodeTable ? "" : "All " ) + String.format("%d</td>\n", missingValue ) );
				fw.write("    <td>Missing value</td>\n");
				fw.write("  </tr>\n");
			}
			fw.write(" </tbody>\n</table>\n");
		}
		catch(Exception ex) {
			ex.printStackTrace();
		}
	}
	
}


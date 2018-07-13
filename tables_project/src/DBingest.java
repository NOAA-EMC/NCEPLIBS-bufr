import java.io.*;
import javax.xml.bind.*;
import javax.xml.validation.*;
import java.sql.*;
import DBconnector.*;
import IDfinder.*;
import descriptorAnalyzer.*;

public class DBingest {
	public static void main(String[] args) {
	    	
	    //  Check for correct number of arguments.
	    if ( args.length != 2 ) {
	    	System.out.println("\nUsage: java DBingest datafile outdir\n");
	    	System.out.println("    where datafile is the [path/]filename of the data file to be ingested");
	    	System.out.println("          outdir   is the [path/]directory where the output listing will be written");
	    	System.exit(-1);
	    }	
	    
	    //  Determine the contents of the datafile.
	    FilenameAnalyzer fna = new FilenameAnalyzer(args[0]);
	    String tbtype = fna.getTableType();
	    if ( tbtype.isEmpty() ) {
	    	System.out.println("Couldn't figure out the table type for filename " + args[0]);
	    	System.exit(-2);
	    }
	    Integer vn = fna.getVersionNumber();
	    String version;
	    if ( vn == -1 ) {
	    	version = "";
	    }
	    else {
	    	version = Integer.toString(vn);
	    }
	    
	    try {
	    	//  Open the database.
	    	DBconnector dbc = new DBconnector();
	    	Connection conn = dbc.getConnection();
	    	
			//  Get the id from the versions table.
	    	IDfinder idf;
	    	if ( tbtype.startsWith("LOCAL") ) {
	    		idf = new IDfinder(conn, vn, fna.getMasterTable(), fna.getOriginatingCenter() );
	    	}
	    	else {
	    		idf = new IDfinder(conn, vn, fna.getMasterTable() );
	    	}
	    	Integer ven = idf.getVersionID();
	    	    
	    	//  Open the output file.
	    	FileWriter fw = new FileWriter(args[1] + "/" + tbtype + ".out");
	    	
	    	if ( args[0].contains(".xml") ) {  //  This is an XML file.
	    		/*
	    		 *  Use the name of the data file to determine the name of the associated XSD document.
	    		 *  The XSD document describes the schema and should be in the same directory as the file itself.
	    		 */
	    		String XSDdoc = args[0].replace(".xml", ".xsd");    	
	    		/*  
	    		 *  Create an unmarshaller from the JAXB binding.  Tables for version 13 should use the same
	    		 *  binding as the corresponding tables from version 14.
	    		 */
	    		String version_jabx = version;
	    		if (vn == 13 || vn == 15 || vn == 16 || vn == 17) {
	    			version_jabx = "14";
	    		}
	    		else {
	    			version_jabx = "18";
	    		}
		    	JAXBContext jc;
	    		if ( args[0].contains("UpdateUnits")) {
	    			jc = JAXBContext.newInstance("TableB_UpdateUnits.jabx");
	    		}
	    		else if ( tbtype.contains("CodeFlag") && vn <= 18 ){
	    			jc = JAXBContext.newInstance(tbtype + ".jabx");
	    		}
	    		else {
	    			jc = JAXBContext.newInstance(tbtype + version_jabx + ".jabx");
	    		}
	    		Unmarshaller u = jc.createUnmarshaller();
	    	    
	    		//  Specify that we want to validate the input file against the XSD document.
	    		SchemaFactory sf = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema");
	    		Schema s = sf.newSchema(new File(XSDdoc));
	    		u.setSchema(s);
	    	    
	    		//  Unmarshal the input file.
	    		Object ob = u.unmarshal(new File(args[0]));
	    	    
	    		//  Process the file. 
	    		if ( tbtype.equals("TableB") ) {
	    			if ( args[0].contains("UpdateUnits")) {
	    				TableB_UpdateUnits_processor tbuup = new TableB_UpdateUnits_processor(ob);
	    				tbuup.writeList(fw);
	    				tbuup.update(fw, ven, conn);
	    			}
	    			else {
		    			switch (vn) {
		    			case 13:
		    			case 14:
		    			case 15:
		    			case 16:
		    			case 17:
		    				TableB14processor tb14p = new TableB14processor(ob);
		    				tb14p.writeList(fw);
		    				tb14p.ingest(fw,ven,conn);
		    				break;
		    			case 18:
		    			case 19:
		    			case 20:
		    			case 21:
		    			case 22:
		    			case 23:
		    			case 24:
		    			case 25:
		    			case 26:
		    			case 27:
		    			case 28:
		    			case 29:
		    			case 30:
		    				TableB18processor tb18p = new TableB18processor(ob);
		    				tb18p.writeList(fw);
		    				tb18p.ingest(fw,ven,conn);
		    				break;
		    			default:
		    				System.out.println("Don't know how to process version " + version + " of " + tbtype);
		    			} 
	    			}
	    		}
		    	else if ( tbtype.equals("TableD") ) {
		    	    switch (vn) {
		    	    case 13:
		    	    case 14:
	    			case 15:
	    			case 16:
	    			case 17:
		    	    	TableD14processor td14p = new TableD14processor(ob);
		    	    	//td14p.writeList(fw);
		    	    	//td14p.ingest(fw, ven, conn);
		    	    	td14p.ingest2(fw, ven, conn);
		    	    	break;
	    			case 18:
	    			case 19:
	    			case 20:
	    			case 21:
	    			case 22:
	    			case 23:
	    			case 24:
	    			case 25:
	    			case 26:
	    			case 27:
	    			case 28:
	    			case 29:
	    			case 30:
		    	    	TableD18processor td18p = new TableD18processor(ob);
		    	    	td18p.writeList(fw);
		    	    	//td18p.ingest(fw, ven, conn);
		    	    	td18p.ingest2(fw, ven, conn);
		    	    	break;	
		    	    default:
		    	    	System.out.println("Don't know how to process version " + version + " of " + tbtype);
		    	    }
		    	}
		    	else if ( tbtype.equals("CodeFlag") ) {
		    	    CodeFlagprocessor cfp = new CodeFlagprocessor(tbtype);
		    	    //CodeFlagprocessor cfp = new CodeFlagprocessor(ob);
		    	    //cfp.writeList(fw);
		    	    //cfp.ingest(fw, conn);
		    	    cfp.copy_and_ingest(fw, conn, vn, ven);
		    	}
		    	else if ( tbtype.equals("C2") ) {
		    		C2processor c2p = new C2processor(ob);
		    		c2p.writeList(fw);
		    		c2p.ingest(fw, conn);
		    	}
		    	else if ( tbtype.equals("C3") ) {
		    		C3processor c3p = new C3processor(ob);
		    		c3p.writeList(fw);
		    		c3p.ingest(fw, conn);
		    	}
		    	else if ( tbtype.equals("C4") ) {
		    		C4processor c4p = new C4processor(ob);
		    		c4p.writeList(fw);
		    		c4p.ingest(fw, conn);
		    	}
		    	else if ( tbtype.equals("C5") ) {
		    		C5processor c5p = new C5processor(ob);
		    		c5p.writeList(fw);
		    		c5p.ingest(fw, conn);
		    	}
		    	else if ( tbtype.equals("C7") ) {
		    		C7processor c7p = new C7processor(ob);
		    		c7p.writeList(fw);
		    		c7p.ingest(fw, conn);
		    	}
		    	else if ( tbtype.equals("C8") ) {
		    		C8processor c8p = new C8processor(ob);
		    		c8p.writeList(fw);
		    		c8p.ingest(fw, conn);
		    	}
		    	else if ( tbtype.equals("C11") ) {
			    	C11processor c11p = new C11processor(ob);
			    	c11p.writeList(fw);
			    	c11p.ingest(fw, conn);
		    	}
		    	else if ( tbtype.equals("C12") ) {
			    	C12processor c12p = new C12processor(ob);
			    	c12p.writeList(fw);
			    	c12p.ingest(fw, conn);
		    	}
		    	else if ( tbtype.equals("C13") ) {
			    	C13processor c13p = new C13processor(ob);
			    	c13p.writeList(fw);
			    	c13p.ingest(fw, conn);
		    	}
		    	else if ( tbtype.equals("C14") ) {
			    	C14processor c14p = new C14processor(ob);
			    	c14p.writeList(fw);
			    	c14p.ingest(fw, conn);
		    	}
		    	else {
		    	    System.out.println("Don't know how to process " + tbtype);
		    	}
	    	}
	    	else if ( args[0].contains(".txt") ) {  //  This is a ".txt" file.
	    		
    			BufferedReader br = new BufferedReader(new FileReader(args[0]));
    			DescriptorAnalyzer da = new DescriptorAnalyzer();
    			String inline;
    			String mnemonic;
    			Integer x, y;
    			
	    		if ( tbtype.equals("LOCALTableB") ) {
	    			String sqlstg_std = "UPDATE bufr_b SET mnemonic = ? " +
	    						"WHERE X = ? AND Y = ? AND version_idx = 19";
	    			String sqlstg_loc = "INSERT INTO bufr_b" +
			         			"(f, x, y, version_idx, element_name, bufr_unit," +
			         			" bufr_scale, bufr_reference_value, bufr_data_width, mnemonic, fxy)" +
			         			"VALUES (0, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
	    			while ( ( inline = br.readLine() ) != null ) {
	    				if ( inline.substring(4,6).contains("0-")  ) {
	    					x = Integer.parseInt(inline.substring(6,8));
	    					y = Integer.parseInt(inline.substring(9,12));
	    					mnemonic = inline.substring(123).trim();
	    					if ( da.isStandard(x, y) ) {
	    						/*
	    						 *  This is a WMO standard descriptor, so we only need to read and store the mnemonic
	    						 *  into the database, because the rest of the information (e.g. scale, bit width, etc.)
	    						 *  has already been read from the WMO tables.
	    						 */
	    						PreparedStatement updateDB = conn.prepareStatement(sqlstg_std);
	    						updateDB.setString(1, mnemonic);
	    						updateDB.setInt(2, x);
	    						updateDB.setInt(3, y);
	    						fw.write("Standard ingest: " + updateDB + "\n");
	    						updateDB.executeUpdate();
	    					}
	    					else {
	    						/*
	    						 *  This is a local descriptor, so we need to read and store all fields into the
	    						 *  database.
	    						 */
	    						PreparedStatement updateDB = conn.prepareStatement(sqlstg_loc);
	    						updateDB.setInt(1, x);
	    						updateDB.setInt(2, y);
	    						updateDB.setInt(3, ven);
	    						updateDB.setString(4, inline.substring(16,68).trim());
	    						updateDB.setString(5, inline.substring(68,94).trim());
	    						updateDB.setString(6, inline.substring(94,97).trim());
	    						updateDB.setString(7, inline.substring(97,110).trim());
	    						updateDB.setString(8, inline.substring(110,121).trim());
	    						updateDB.setString(9, mnemonic);
	    						updateDB.setString(10, "0" + inline.substring(6,8) + inline.substring(9,12));
	    						fw.write("Local ingest: " + updateDB + "\n");
	    						//updateDB.executeUpdate();
	    					}
	    				}
	    			}
	    		}
	    		else if ( tbtype.equals("LOCALTableD") ) {
	    			String sqlstg_std = "UPDATE bufr_d SET mnemonic = ? WHERE X = ? AND Y = ? AND version_idx = 19";
	    			String sqlstg_loc = "INSERT INTO bufr_d (fxy, f, x, y, version_idx, mnemonic, cat_seq, ref_fxy, " +
	    					"ref_tb_f, ref_tb_x, ref_tb_y, order_idx) VALUES (?, 3, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
	    			while ( ( inline = br.readLine() ) != null ) {
	    				if ( inline.substring(0,2).contains("3-")  ) {
	    					x = Integer.parseInt(inline.substring(2,4));
	    					y = Integer.parseInt(inline.substring(5,8));
	    					if ( da.isStandard(x, y) ) {
	    						/*
	    						 *  This is a WMO standard descriptor, so we only need to read and store the mnemonic
	    						 *  into the database, because the rest of the information (e.g. scale, bit width, etc.)
	    						 *  has already been read from the WMO tables.
	    						 */
	    						PreparedStatement updateDB = conn.prepareStatement(sqlstg_std);
	    						updateDB.setString(1, inline.substring(9).trim());
	    						updateDB.setInt(2, x);
	    						updateDB.setInt(3, y);
	    						fw.write("Standard ingest: " + updateDB + "\n");
	    						updateDB.executeUpdate();
	    					}
	    					else {
	    						/*
	    						 *  This is a local descriptor, so we need to read and store all fields into the
	    						 *  database.
	    						 */
	    						PreparedStatement updateDB = conn.prepareStatement(sqlstg_loc);
	    						updateDB.setString(1, "3" + inline.substring(2,4) + inline.substring(5,8));
	    						updateDB.setInt(2, x);
	    						updateDB.setInt(3, y);
	    						updateDB.setInt(4, ven);
	    						updateDB.setString(5, inline.substring(9,17).trim());
	    						updateDB.setString(6, inline.substring(18,70).trim());
	    						String[] ref_fxy = inline.substring(70).split(",");
	    						for ( int i = 0; i < ref_fxy.length; i++ ) {
	    							updateDB.setString(7, ref_fxy[i].trim());
	    							updateDB.setInt(8, Integer.parseInt(da.getF(ref_fxy[i].trim())));
	    							updateDB.setInt(9, Integer.parseInt(da.getX(ref_fxy[i].trim())));
	    							updateDB.setInt(10, Integer.parseInt(da.getY(ref_fxy[i].trim())));
	    							updateDB.setInt(11, i+1);
	    							fw.write("Local ingest: " + updateDB + "\n");
		    						//updateDB.executeUpdate();
	    						}
	    					}
	    				}
	    			}
	    			/*
	    			 *  Fill in the bufr_b_idx, ref_mnemonic and ref_element_name fields for any local Table D
	    			 *  descriptors.  Note that in this call to the td18p.ingest2 method, ven always points to
	    			 *  a local table in the database.
	    			 */
	    			//TableD18processor td18p = new TableD18processor();
	    			//td18p.ingest2(fw, ven, conn);
	    		}
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
}

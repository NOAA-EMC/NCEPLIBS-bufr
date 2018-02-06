import java.io.*;
import java.sql.*;
import java.util.*;

import TableD14.jabx.*;

public class TableD14processor {
	
	protected List<ExportingBUFRTableDE> tbentries;

	//  Constructor methods.
	
	public TableD14processor() {
		tbentries.clear();
	}
	
	public TableD14processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getExportingBUFRTableDE();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Table D entries.
	
	public void writeList(FileWriter fw) {
		String fxy1, fxy2, fxy1prev;
		String outline;
		
		if ( tbentries.size() > 0 ) {
			try {
				fxy1prev = "";
				for (ExportingBUFRTableDE tbentry : tbentries) {
			    	fxy1 = tbentry.getFXY1();
			    	fxy2 = tbentry.getFXY2();
			    	if ( ! fxy1prev.equals(fxy1) ) {  // Beginning of new Table D entry.
			    		outline = String.format("\nEntry #%5d %3s %s-%s-%s %15s  >%s<\n",
				    			tbentry.getNo(), tbentry.getCategory(),
				    			fxy1.substring(0,1), fxy1.substring(1,3), fxy1.substring(3,6),
				    			tbentry.getStatus(), tbentry.getElementName1());
			    		fw.write(outline);
			    	}
			    	outline = String.format("Entry #%5s     %s-%s-%s  >%s<\n",
			    			tbentry.getNo(),
			    			fxy2.substring(0,1), fxy2.substring(1,3), fxy2.substring(3,6),
			    			tbentry.getElementName2());
			    	fxy1prev = fxy1;
			    	fw.write(outline);
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }	
		}
		else {
			System.out.println("There were no TableD entries in the list!");
		}
		
	}

	//  Method to ingest all Table D entries into the bufr_d table of the database.
	
	public void ingest(FileWriter fw, Integer ven, Connection cn) {	
	
		String fxy1, fxy1prev, fxy2;
		String sqlstg1, sqlstg2;
		String workstg;
		PreparedStatement updateDB;
		Integer ict = 1;
		
		if ( tbentries.size() > 0 ) {
			try {
				fxy1prev = "";
				sqlstg1 = "INSERT INTO bufr_d " +
		         "(fxy, f, x, y, version_idx, order_idx, ref_fxy, ref_tb_f, ref_tb_x, ref_tb_y, category) " +
		         "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
				sqlstg2 = "INSERT INTO bufr_d " +
		         "(fxy, f, x, y, version_idx, order_idx, ref_fxy, ref_tb_f, ref_tb_x, ref_tb_y, category, cat_seq) " +
		         "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
				for (ExportingBUFRTableDE tbentry : tbentries) {	
			    	fxy1 = tbentry.getFXY1();
			    	if ( ! fxy1prev.equals(fxy1) ) {  // Beginning of new Table D entry.
			    		ict = 1;
			    		fxy1prev = fxy1;
			    	}
		    		if (tbentry.getElementName1() == null) {
		    			updateDB = cn.prepareStatement(sqlstg1);
		    		}
		    		else {  // Get the SequenceName, but remove the leading and trailing parentheses.
		    			updateDB = cn.prepareStatement(sqlstg2);
		    			workstg = tbentry.getElementName1().substring(1);
		    			updateDB.setString(12, workstg.substring(0, workstg.length()-1));
		    		}
		    		updateDB.setString(1, fxy1);
		    		updateDB.setInt(2, Integer.parseInt(fxy1.substring(0,1)));
		    		updateDB.setInt(3, Integer.parseInt(fxy1.substring(1,3)));
		    		updateDB.setInt(4, Integer.parseInt(fxy1.substring(3,6)));
		    		updateDB.setInt(5, ven);
		    		updateDB.setInt(6, ict++);
		    		fxy2 = tbentry.getFXY2();
		    		updateDB.setString(7, fxy2);
		    		updateDB.setInt(8, Integer.parseInt(fxy2.substring(0,1)));
		    		updateDB.setInt(9, Integer.parseInt(fxy2.substring(1,3)));
		    		updateDB.setInt(10, Integer.parseInt(fxy2.substring(3,6)));
		    		updateDB.setString(11, tbentry.getCategoryOfSequences());
		    		if ( ! tbentry.getStatus().contains("alidatio") ) {
			    		fw.write(updateDB.toString() + "\n");  // Write copy of SQL command to output file.
		    			updateDB.executeUpdate();
			    	}
				}
			}
		    catch(Exception ex) {
		    	ex.printStackTrace();
		    }
		}
		else {
			System.out.println("There were no TableD entries in the list!");
		}
		
	}

	/*
	 * Method to populate the tb_id_fk, ref_mnemonic and ref_element_name fields of the bufr_d table, BUT ONLY AFTER:
	 *   1.  the above ingest method has already been run to create the entries themselves
	 *   2.  DBingest.java has already been run on a "LOCALTableD*" file to populate the mnemonic field of the bufr_d table
	 */
	
	public void ingest2(FileWriter fw, Integer ven, Connection cn) {	
	
		PreparedStatement getEntry, getEntry2, updateDB;
		ResultSet rs, rs2;
		
		Integer ref_f, ref_x, ref_y;
		Integer en_seq, en_elem;
		Integer id;
		String ref_fxy;
		String opNx, opNy;
		
		EntryNumberFinder enf = new EntryNumberFinder (fw, cn, ven);
		
		try {	
			
			getEntry = cn.prepareStatement("SELECT id, ref_fxy FROM bufr_d WHERE version_idx = ?");
			getEntry.setInt(1, ven);
			rs = getEntry.executeQuery();
			
			while (rs.next()) {  // Loop through each entry in the result set.
				id = rs.getInt("id");
				ref_fxy = rs.getString("ref_fxy");
				ref_f = Integer.parseInt(ref_fxy.substring(0,1));
				ref_x = Integer.parseInt(ref_fxy.substring(1,3));
				ref_y = Integer.parseInt(ref_fxy.substring(3,6));
				switch ( ref_f ) {
				case 0:
					en_elem = enf.getEntryNumber("bufr_b", ref_fxy);
					if ( en_elem != -1 ) {
						getEntry2 = cn.prepareStatement("SELECT mnemonic, element_name from bufr_b where id = ?");
						getEntry2.setInt(1, en_elem);
						rs2 = getEntry2.executeQuery();
						rs2.next();
						updateDB = cn.prepareStatement("UPDATE bufr_d SET tb_id_fk = ?, ref_mnemonic = ?, ref_element_name = ? WHERE id = ?");
						updateDB.setInt(1, en_elem);
						updateDB.setString(2, rs2.getString("mnemonic"));
						updateDB.setString(3, rs2.getString("element_name"));
						updateDB.setInt(4, id);
						fw.write(updateDB.toString() + "\n");
						updateDB.executeUpdate();
					}
					break;
				case 1:
					updateDB = cn.prepareStatement("UPDATE bufr_d SET ref_mnemonic = ?, ref_element_name = ? WHERE id = ?");
					updateDB.setString(1, ref_fxy);
					updateDB.setInt(3, id);
					if ( ref_x == 1 ) {
						opNx = " descriptor";
					}
					else {
						opNx = " descriptors";
					}
					if ( ref_y == 0 ) {
						updateDB.setString(2, "Delayed replication of " + ref_x.toString() + opNx);
					}
					else {
						if ( ref_y == 1 ) {
							opNy = " time";
						}
						else {
							opNy = " times";
						}
						updateDB.setString(2, "Replicate " + ref_x.toString() + opNx + " " + ref_y.toString() + opNy);
					}
					fw.write(updateDB.toString() + "\n");
					updateDB.executeUpdate();
					break;
				case 2:
					updateDB = cn.prepareStatement("UPDATE bufr_d SET ref_mnemonic = ?, ref_element_name = ? WHERE id = ?");
					updateDB.setString(1, ref_fxy);
					updateDB.setInt(3, id);
					switch ( ref_x ) {
					case 1:
						if ( ref_y == 0 ) {
							updateDB.setString(2, "Cancel change data width");
						}
						else {
							updateDB.setString(2, "Change data width");
						}
						break;
					case 2:
						if ( ref_y == 0 ) {
							updateDB.setString(2, "Cancel change scale");
						}
						else {
							updateDB.setString(2, "Change scale");
						}
						break;
					case 4:
						if ( ref_y == 0 ) {
							updateDB.setString(2, "Cancel add associated field");
						}
						else {
							updateDB.setString(2, "Add associated field");
						}
						break;
					case 5:
						updateDB.setString(2, "Add character field");
						break;
					case 7:
						if ( ref_y == 0 ) {
							updateDB.setString(2, "Cancel increase scale, reference value and data width");
						}
						else {
							updateDB.setString(2, "Increase scale, reference value and data width");
						}
						break;
					default:
						updateDB.setString(2, "Table C operator unknown to TableD14processor.java code");
						break;
					}
					fw.write(updateDB.toString() + "\n");
					updateDB.executeUpdate();
				default:  // ( ref_f == 3 )
					en_seq = enf.getEntryNumber("bufr_d", ref_fxy);
					if ( en_seq != -1 ) {
						getEntry2 = cn.prepareStatement("SELECT mnemonic, cat_seq from bufr_d where id = ?");
						getEntry2.setInt(1, en_seq);
						rs2 = getEntry2.executeQuery();
						rs2.next();
						if ( rs2.getString("cat_seq") == null ) {
							updateDB = cn.prepareStatement("UPDATE bufr_d SET ref_mnemonic = ? WHERE id = ?");
							updateDB.setInt(2, id);
						}
						else {
							updateDB = cn.prepareStatement("UPDATE bufr_d SET ref_mnemonic = ?, ref_element_name = ? WHERE id = ?");
							updateDB.setString(2, rs2.getString("cat_seq"));
							updateDB.setInt(3, id);
						}
						updateDB.setString(1, rs2.getString("mnemonic"));
						fw.write(updateDB.toString() + "\n");
						updateDB.executeUpdate();
					}
					break;
				}
			}
		}

		catch(Exception ex) {
		    	ex.printStackTrace();
		}
		
	}
	
}

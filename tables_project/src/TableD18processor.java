import java.io.*;
import java.sql.*;
import java.util.*;
import descriptorAnalyzer.*;

import TableD18.jabx.*;

public class TableD18processor {
	
	protected List<BUFR1800TableDEn> tbentries;

	//  Constructor methods.
	
	public TableD18processor() {
		//tbentries.clear();
	}
	
	public TableD18processor(Object ob) {
		if ( ob instanceof Dataroot ) {
	        Dataroot tb = (Dataroot) ob;
	        tbentries = tb.getBUFR1800TableDEn();
		}
		else {
			tbentries.clear();
		}
	}
	
	//  Method to write an ASCII listing of all Table D entries.
	
	public void writeList(FileWriter fw) {
		String fxy1, fxy2, fxy1prev;
		String outline;
		DescriptorAnalyzer da = new DescriptorAnalyzer();
		
		if ( tbentries.size() > 0 ) {
			try {
				fxy1prev = "";
				for (BUFR1800TableDEn tbentry : tbentries) {
			    	fxy1 = tbentry.getFXY1();
			    	fxy2 = tbentry.getFXY2();
			    	if ( ! fxy1prev.equals(fxy1) ) {  // Beginning of new Table D entry.
			    		outline = String.format("\nEntry #%5d %3s %s-%s-%s %15s  >%s<\n",
				    			tbentry.getNo().intValue(), tbentry.getCategory(),
				    			da.getF(fxy1), da.getX(fxy1), da.getY(fxy1),
				    			tbentry.getStatus(), tbentry.getTitleEn());
			    		fw.write(outline);
			    	}
			    	outline = String.format("Entry #%5s     %s-%s-%s  >%s<\n",
			    			tbentry.getNo().intValue(),
			    			da.getF(fxy2), da.getX(fxy2), da.getY(fxy2),
			    			tbentry.getElementNameEn());
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
		DescriptorAnalyzer da = new DescriptorAnalyzer();
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
				for (BUFR1800TableDEn tbentry : tbentries) {	
			    	fxy1 = tbentry.getFXY1();
			    	if ( ! fxy1prev.equals(fxy1) ) {  // Beginning of new Table D entry.
			    		ict = 1;
			    		fxy1prev = fxy1;
			    	}
		    		if (tbentry.getTitleEn() == null) {
		    			updateDB = cn.prepareStatement(sqlstg1);
		    		}
		    		else {  // Get the sequence name, but remove the leading and trailing parentheses.
		    			updateDB = cn.prepareStatement(sqlstg2);
		    			workstg = tbentry.getTitleEn().substring(1);
		    			updateDB.setString(12, workstg.substring(0, workstg.length()-1));
		    		}
		    		updateDB.setString(1, fxy1);
		    		updateDB.setInt(2, Integer.parseInt(da.getF(fxy1)));
		    		updateDB.setInt(3, Integer.parseInt(da.getX(fxy1)));
		    		updateDB.setInt(4, Integer.parseInt(da.getY(fxy1)));
		    		updateDB.setInt(5, ven);
		    		updateDB.setInt(6, ict++);
		    		fxy2 = tbentry.getFXY2();
		    		updateDB.setString(7, fxy2);
		    		updateDB.setInt(8, Integer.parseInt(da.getF(fxy2)));
		    		updateDB.setInt(9, Integer.parseInt(da.getX(fxy2)));
		    		updateDB.setInt(10, Integer.parseInt(da.getY(fxy2)));
		    		updateDB.setString(11, tbentry.getCategoryOfSequencesEn());
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
	 * Method to populate the bufr_b_idx, ref_mnemonic and ref_element_name fields of the bufr_d table
	 */
	
	public void ingest2(FileWriter fw, Integer ven, Connection cn) {	
	
		PreparedStatement getEntry, getEntry2, updateDB;
		ResultSet rs, rs2;
		
		Integer ref_f, ref_x, ref_y;
		Integer x, y;
		Integer en_seq, en_elem;
		Integer id;
		String ref_fxy;
		String fxy;
		String opNx, opNy;
		
		EntryNumberFinder enf = new EntryNumberFinder (fw, cn, ven);
		DescriptorAnalyzer da = new DescriptorAnalyzer();
		
		try {	
			
			getEntry = cn.prepareStatement("SELECT id, fxy, ref_fxy FROM bufr_d WHERE version_idx = ?");
			getEntry.setInt(1, ven);
			rs = getEntry.executeQuery();
			
			while (rs.next()) {  // Loop through each entry in the result set.
				id = rs.getInt("id");
				fxy = rs.getString("fxy");
				ref_fxy = rs.getString("ref_fxy");
				ref_f = Integer.parseInt(da.getF(ref_fxy));
				ref_x = Integer.parseInt(da.getX(ref_fxy));
				ref_y = Integer.parseInt(da.getY(ref_fxy));
				x = Integer.parseInt(da.getX(fxy));
				y = Integer.parseInt(da.getY(fxy));
				switch ( ref_f ) {
				case 0:
					/* 
					 * ref_fxy is a Table B descriptor.  Check that ref_fxy and fxy are both standard descriptors
					 * or else that both are local descriptors before updating the ref_mnemonic, ref_element_name
					 * and bufr_b_idx fields for this entry.  Local Table D descriptors can contain standard Table B
					 * descriptors as sequence members, and for such entries we have no way of knowing ahead of time
					 * which version of the standard tables may be specified in any given BUFR message which uses
					 * these local Table D entries.  So we don't want to tie the ref_mnemonic, ref_element_name and
					 * bufr_b_idx fields of these entries to any particular version of the standard tables.
					 */
					if (  ( ( da.isStandard(x, y) ) && ( da.isStandard(ref_x, ref_y) ) )
								||
						  ( ( da.isLocal(x, y) ) && ( da.isLocal(ref_x, ref_y) ) )  ) {
						en_elem = enf.getEntryNumber("bufr_b", ref_fxy);
						if ( en_elem != -1 ) {
							getEntry2 = cn.prepareStatement("SELECT mnemonic, element_name from bufr_b where id = ?");
							getEntry2.setInt(1, en_elem);
							rs2 = getEntry2.executeQuery();
							rs2.next();
							updateDB = cn.prepareStatement("UPDATE bufr_d SET bufr_b_idx = ?, ref_mnemonic = ?," +
									"ref_element_name = ? WHERE id = ?");
							updateDB.setInt(1, en_elem);
							updateDB.setString(2, rs2.getString("mnemonic"));
							updateDB.setString(3, rs2.getString("element_name"));
							updateDB.setInt(4, id);
							fw.write(updateDB.toString() + "\n");
							updateDB.executeUpdate();
						}
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
					case 8:
						if ( ref_y == 0 ) {
							updateDB.setString(2, "Cancel change width of CCITT IA5 field");
						}
						else {
							updateDB.setString(2, "Change width of CCITT IA5 field");
						}
						break;
					default:
						updateDB.setString(2, "Table C operator unknown to TableD18processor.java code");
						break;
					}
					fw.write(updateDB.toString() + "\n");
					updateDB.executeUpdate();
					break;
				default:  // ( ref_f == 3 )
					/* 
					 * ref_fxy is a Table D descriptor.  Check that ref_fxy and fxy are both standard descriptors
					 * or else that both are local descriptors before updating the ref_mnemonic and ref_element_name
					 * fields for this entry.  Local Table D descriptors can contain standard Table D descriptors as
					 * sequence members, and for such entries we have no way of knowing ahead of time which version
					 * of the standard tables may be specified in any given BUFR message which uses these local
					 * Table D entries.  So we don't want to tie the ref_mnemonic and ref_element_name fields of
					 * these entries to any particular version of the standard tables.
					 */
					if (  ( ( da.isStandard(x, y) ) && ( da.isStandard(ref_x, ref_y) ) )
							    ||
						  ( ( da.isLocal(x, y) ) && ( da.isLocal(ref_x, ref_y) ) )  ) {
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
								updateDB = cn.prepareStatement("UPDATE bufr_d SET ref_mnemonic = ?," +
										"ref_element_name = ? WHERE id = ?");
								updateDB.setString(2, rs2.getString("cat_seq"));
								updateDB.setInt(3, id);
							}
							updateDB.setString(1, rs2.getString("mnemonic"));
							fw.write(updateDB.toString() + "\n");
							updateDB.executeUpdate();
						}
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

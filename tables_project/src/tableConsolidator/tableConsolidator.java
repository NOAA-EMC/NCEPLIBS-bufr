package tableConsolidator;

import java.util.ArrayList;

public class tableConsolidator {
	/*
	**	for code tables where "depends_on" is not null and where the same code figures and meanings are shared by multiple
	**	"depends_on" values, this class consolidates those multiple values into a single bufrlib entry or HTML table
	*/
	protected ArrayList<ArrayList<Integer>> mygroups;
	
	protected int ngroups;
	
	protected int idx;
	
	protected String htmlTables[], consolidatedTable;  
	
	protected boolean lastValueinGroup;
	
	public tableConsolidator ( ArrayList<Integer>... groups ) {
		/*
		**	Constructor method.
		**
		**	groups is an input list of integer arrays, where each array (i.e. "group") contains all of the "depends_on" values
		**	for which the same code figures and meanings are to be consolidated into a single bufrlib entry or HTML table
		*/
		try {
			
			/*
			**	Determine a count of the number of input groups, and store each group into a local doubly-dimensioned ArrayList
			*/
			mygroups = new ArrayList<ArrayList<Integer>>();
			mygroups.clear();
			for ( ArrayList<Integer> group : groups ) {
				mygroups.add(group);
			}
			ngroups = mygroups.size();
			
			/*
			**	each String in the htmlTables array will contain the bufrlib entry or HTML table code corresponding to one of the input groups
			*/
			htmlTables = new String[ngroups];
			for ( int i = 0; i < htmlTables.length ; i++ ) {
				htmlTables[i] = null;
			}
			
		}
		catch(Exception ex) {
			ex.printStackTrace();
		}
	}
	
	public boolean processNewValue ( Integer format, Integer dponValue, String meaning, String mnemonic, String f, String x, String y, String elementName ) {
		/*
		**	all arguments are input:
		**		format = 1 iff bufrlib, 3 iff html
		**		dponValue = "depends_on" value
		**		meaning = meaning associated with dponValue
		**		mnemonic = Table B mnemonic for which dponValue is a code figure
		**		f = F value associated with mnemonic
		**		x = X value associated with mnemonic
		**		y = Y value associated with mnemonic
		**		elementName = element name associated with mnemonic
		**	the return value is true iff dponValue was the last remaining value in the associated group
		*/
		try {
			lastValueinGroup = false;
			for ( ArrayList<Integer> group : mygroups ) {  // find the group to which dponValue belongs
				if ( group.contains(dponValue) ) {
					idx = mygroups.indexOf(group);
					if ( htmlTables[idx] == null ) {  // this is the first time this method has been called for the associated group
						if ( format == 1 ) {
							htmlTables[idx] = String.format("           | %s-%s-%s=%d", f, x, y, dponValue );
						}
						else {
							htmlTables[idx] = String.format("<br />\n<p class=\"left\">When the value of<br>\n" +
									"<span class=\"bold indent\">%s-%s-%s - %s (%s)<br><br>\n= %d (%s)</span><br>\n",
		    						f, x, y, mnemonic, elementName, dponValue, meaning );
						}
					}
					else {
						if ( format == 1 ) {
							htmlTables[idx] += String.format(",%d", dponValue );
						}
						else {
							htmlTables[idx] += String.format("<span class=\"indentX2\">or</span><br>\n" +
									"<span class=\"bold\">\n= %d (%s)</span><br>\n",
	    							dponValue, meaning );
						}
					}
					group.remove(dponValue);
					if ( group.isEmpty() ) {  // dponValue was the last remaining value in the associated group
						htmlTables[idx] += ( format == 3 ? "</p>" : "" ) + "\n";
						lastValueinGroup = true;
						consolidatedTable = htmlTables[idx];
					}
					else {
						consolidatedTable = "";
					}
				}
			}
		}
		catch(Exception ex) {
			ex.printStackTrace();
		}
		return lastValueinGroup;
	}
	
	public String getConsolidatedTable() {
		/*
		**	Getter method.
		*/
		return consolidatedTable;
	}
	
}
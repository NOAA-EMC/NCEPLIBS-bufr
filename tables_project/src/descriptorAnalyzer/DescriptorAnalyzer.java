package descriptorAnalyzer;

public class DescriptorAnalyzer {

	// Constructor method.
	public DescriptorAnalyzer() {
	}
	
	public boolean isStandard(Integer x, Integer y) {
		return ( x < 48 && y < 192 );
	}
	
	public boolean isLocal(Integer x, Integer y) {
		return ( x >= 48 || y >= 192 );
	}
	
	public String getF(String fxy) {
		return ( fxy.length() == 6 ? fxy.substring(0,1) : "" );
	}
	
	public String getX(String fxy) {
		return ( fxy.length() == 6 ? fxy.substring(1,3) : "" );
	}
	
	public String getY(String fxy) {
		return ( fxy.length() == 6 ? fxy.substring(3) : "" );
	}
}

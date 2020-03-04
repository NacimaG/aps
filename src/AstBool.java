
public class AstBool implements Ast {
	boolean val;
	
	AstBool( boolean b) {
		val = b ;
	}

	@Override
	public String toPrologString() {
		return ("bool(" + val + ")");
	}

}



public enum Op {
	ADD("add"), SUB("sub"), MUL("mul"), DIV("div"), 
	AND("and"),NOT("not"), OR("or"), 
	EQUALS("eq"), LESSTHAN("lt");
	
	private String str;

	Op(String str) {
		this.str = str;
	}

	public String toString() {
		return this.str;
	}
}
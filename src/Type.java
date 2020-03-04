
public enum Type {
	BOOL("bool"), INT("int");
	
	private String str;

	Type(String str) {
		this.str = str;
	}

	public String toString() {
		return this.str;
	}
}

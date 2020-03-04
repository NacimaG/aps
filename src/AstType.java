import java.util.ArrayList;

public class AstType implements Ast {
	Type type;
	ArrayList<Ast> declare;

	AstType(Type type, ArrayList<Ast> es) {
		this.type = type;
		this.declare = es;
	}

	@Override
	public String toPrologString() {
		String r = "";
		r = type.toString() + "([";
		for (int i = 0; i < declare.size() - 1; i++)
			r += declare.get(i).toPrologString() + ",";
		r += declare.get(declare.size() - 1).toPrologString();
		r += "])";
		return r;
	}
}
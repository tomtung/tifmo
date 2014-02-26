package res.ja;

public class JaGoiEntry implements java.io.Serializable {
	
	public final long entryID;
	public final String lemma;
	public final String pos;
	public final String[] categories;
	public final String[][] frame;
	public final String[][] args;
	//public final boolean thcl;
	
	private String str;
	private void mkString() {
		StringBuilder ret = new StringBuilder();
		ret.append(String.format("%08x", entryID));
		ret.append("[");
		ret.append(pos);
		ret.append("]");
		if (categories != null) {
			for (int i = 0; i < categories.length; i++) {
				ret.append(" < ");
				ret.append(categories[i]);
			}
		}
		ret.append("\n");
		if (frame != null) {
			for (int i = 0; i < frame.length; i++) {
				if (args[i] != null) {
					ret.append("(");
					for (int j = 0; j < frame[i].length; j++) {
						ret.append(frame[i][j]);
						if (j <= frame[i].length - 2) ret.append(" | ");
					}
					ret.append(")");
					for (int j = 0; j < args[i].length; j++) {
						ret.append(args[i][j]);
						if (j <= args[i].length - 2) ret.append("/");
					}
				} else {
					ret.append(frame[i][0]);
				}
				ret.append(" ");
			}
			ret.append(lemma);
			//if (thcl) ret.append(" (th-cl)");
		} else {
			ret.append(lemma);
		}
		str = ret.toString();
	}
	private int hash;
	
	public JaGoiEntry(long entryID, String lemma, String pos, String[] categories, String[][] frame, String[][] args) {
		this.entryID = entryID;
		this.lemma = lemma;
		this.pos = pos;
		this.categories = categories;
		this.frame = frame;
		this.args = args;
		//this.thcl = thcl;
		mkString();
		hash = str.hashCode();
	}
	
	@Override
	public boolean equals(Object a) {
		if (a instanceof JaGoiEntry) {
			JaGoiEntry that = (JaGoiEntry)a;
			return (this == that) || str.equals(that.str);
		} else {
			return false;
		}
	}
	
	@Override
	public int hashCode() {
		return hash;
	}
	
	@Override
	public String toString() {
		return str;
	}
}

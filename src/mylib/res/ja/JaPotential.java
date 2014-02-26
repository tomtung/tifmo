package res.ja;

import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.HashMap;

public class JaPotential {
	
	private static HashMap<String, String> potential = null;
	private static boolean initialized = initialize();
	
	private static boolean initialize() {
		boolean ret = false;
		try {
			String path = JaPotential.class.getClassLoader().getResource("data/ja/JaPotential/dic.obj").getPath();
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(path));
			potential = (HashMap<String, String>)ois.readObject();
			ois.close();
			ret = true;
		} catch(Exception e) {
			System.err.println("cannot read dic.obj.");
		}
		return ret;
	}
	
	public static String lookup(String word) {
		if (potential.containsKey(word)) {
			return potential.get(word);
		} else {
			return null;
		}
	}
	
}

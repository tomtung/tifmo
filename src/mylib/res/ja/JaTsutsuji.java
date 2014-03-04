package mylib.res.ja;

import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.HashMap;

public class JaTsutsuji {
	
	private static HashMap<String, String[]> mapID = null;
	private static HashMap<String, String[]> mapMC = null;
	private static boolean initialized = initialize();
	
	private static boolean initialize() {
		boolean ret = false;
		try {
			String path = JaTsutsuji.class.getClassLoader().getResource("data/ja/Tsutsuji/dic.obj").getPath();
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(path));
			mapID = (HashMap<String, String[]>)ois.readObject();
			mapMC = (HashMap<String, String[]>)ois.readObject();
			ois.close();
			ret = true;
		} catch(Exception e) {
			System.err.println("cannot read dic.obj.");
		}
		return ret;
	}
	
	public static String[] lookupID(String word) {
		if (mapID.containsKey(word)) {
			return mapID.get(word);
		} else {
			return null;
		}
	}
	
	public static String[] lookupMC(String word) {
		if (mapMC.containsKey(word)) {
			return mapMC.get(word);
		} else {
			return null;
		}
	}
	
}

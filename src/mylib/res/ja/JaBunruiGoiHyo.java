package res.ja;

import com.strangegizmo.cdb.Cdb;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.HashMap;

public class JaBunruiGoiHyo {
	
	private static Cdb bunrui = null;
	private static HashMap<String, String> stdmap = null;
	
	private static boolean initialized = initialize();
	
	private static boolean initialize() {
		boolean ret = false;
		try {
			String jnpath = JaBunruiGoiHyo.class.getClassLoader().getResource("data/ja/JumanNorm/dic.obj").getPath();
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(jnpath));
			stdmap = (HashMap<String, String>)ois.readObject();
			ois.close();
			String cdbpath = JaBunruiGoiHyo.class.getClassLoader().getResource("data/ja/BunruiGoiHyoZOHOBAN/BUNRUIDB.cdb").getPath();
			bunrui = new Cdb(cdbpath);
			
			ret = true;
		} catch(Exception e) {
			System.err.println("cannot find database");
		}
		return ret;
	}
	
	public static String[] lookup(String word) {
		try {
			byte[] bw = null;
			if (stdmap.containsKey(word)) {
				bw = stdmap.get(word).getBytes("UTF-8");
			} else {
				bw = word.getBytes("UTF-8");
			}
			//bunrui.findstart(bw);
			byte[] ret = bunrui.find(bw);
			if (ret != null) {
				return (new String(ret, "UTF-8")).split("\t");
			} else {
				return null;
			}
		} catch(UnsupportedEncodingException e) {
		}
		return null;
	}
}

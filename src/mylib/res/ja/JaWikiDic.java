package res.ja;

import com.strangegizmo.cdb.Cdb;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.TreeSet;
import java.util.HashSet;

public class JaWikiDic {

	private static Cdb wikicats = null;
	private static Cdb wikidic = null;
	private static Cdb wikired = null;
	private static Cdb wikihyper = null;
	private static boolean initialized = initialize();

	private static boolean initialize() {
		boolean ret = false;
		try {
			String path = JaWikiDic.class.getClassLoader().getResource("data/ja/Wikipedia").getPath();
			wikicats = new Cdb(path + "/WikiCats.cdb");
			wikidic = new Cdb(path + "/WikiDic.cdb");
			wikired = new Cdb(path + "/WikiRed.cdb");
			wikihyper = new Cdb(path + "/WikiHyper.cdb");
			ret = true;
		} catch(IOException e) {
			System.err.println("cannot find database");
		}
		return ret;
	}

	public static String[] lookup(String word) {
		try {
			byte[] bw = word.getBytes("UTF-8");
			//wikired.findstart(bw);
			byte[] rred = wikired.find(bw);
			byte[] rdic = null;
			if (rred == null) {
				//wikidic.findstart(bw);
				rdic = wikidic.find(bw);
			} else {
				//wikidic.findstart(rred);
				rdic = wikidic.find(rred);
			}
			if (rdic == null) {
				return null;
			} else {
				String[] cat = (new String(rdic, "UTF-8")).split(",");
				TreeSet<String> ret = new TreeSet<String>();
				for (int i = 0; i < cat.length; i++) {
					ret.add(cat[i]);
					byte[] bcat = cat[i].getBytes("UTF-8");
					//wikicats.findstart(bcat);
					byte[] bsup = wikicats.find(bcat);
					if (bsup != null) {
						String[] sup = (new String(bsup, "UTF-8")).split(",");
						for (int j = 0; j < sup.length; j++) {
							ret.add(sup[j]);
						}
					}
				}
				return (String[])ret.toArray(new String[ret.size()]);
			}
		} catch(UnsupportedEncodingException e) {
		}
		return null;
	}


	private static String redir(String word) {
		try {
			byte[] bw = word.getBytes("UTF-8");
			//wikired.findstart(bw);
			byte[] rred = wikired.find(bw);
			if (rred != null) return new String(rred, "UTF-8");
		} catch(UnsupportedEncodingException e) {
		}
		return word;
	}

	public static boolean sameEntry(String w1, String w2) {
		return redir(w1).equals(redir(w2));
	}

	private static HashSet<String> stopwords = initwordlist();

	private static HashSet<String> initwordlist() {
		HashSet<String> ret = new HashSet<String>();
		ret.add("できる");
		ret.add("かつて");
		ret.add("過去");
		ret.add("存在");
		ret.add("一般");
		ret.add("可能");
		ret.add("地方");
		ret.add("出身");
		ret.add("重要");
		ret.add("合法");
		ret.add("独立");
		ret.add("日本");
		ret.add("日本語");
		ret.add("外国");
		ret.add("外国人");
		ret.add("置く");
		ret.add("行う");
		ret.add("ある");
		ret.add("いる");
		ret.add("代表");
		ret.add("使用");
		ret.add("登場");
		ret.add("世界");
		ret.add("統治");
		ret.add("軍事");
		ret.add("能力");
		ret.add("実施");
		ret.add("公用語");
		ret.add("範囲");
		ret.add("現在");
		ret.add("採用");
		ret.add("利用");
		ret.add("異教");
		ret.add("展開");
		ret.add("初期");
		ret.add("開発");
		ret.add("禁止");
		ret.add("影響");
		ret.add("テレビ");
		ret.add("参加");
		ret.add("大きな");
		ret.add("多い");
		ret.add("問題");
		ret.add("加盟");
		ret.add("駐留");
		ret.add("派遣");
		ret.add("締結");
		ret.add("連合");
		ret.add("援助");
		ret.add("発足");
		ret.add("承認");
		ret.add("東西");
		ret.add("南北");
		ret.add("担当");
		ret.add("進出");
		ret.add("反対");
		ret.add("国際");
		ret.add("種類");
		ret.add("関係");
		ret.add("分野");
		ret.add("用語");
		ret.add("登場人物");
		return ret;
	}

	private static boolean stop(String w) {
		return w.length() <= 1 || stopwords.contains(w) || w.indexOf("する") != -1;
	}

	public static boolean isHyper(String w1, String w2) {
		if (stop(w1) || stop(w2)) return false;
		if (w1.indexOf(w2) != -1 && !w1.substring(w1.length() - w2.length(), w1.length()).equals(w2)) return false;
		//if (lookup(w1) != null && lookup(w2) != null && w1.indexOf(w2) != -1) return true;
		try {
			byte[] bw = w1.getBytes("UTF-8");
			//wikired.findstart(bw);
			byte[] rred = wikired.find(bw);
			byte[] rhyper = null;
			if (rred == null) {
				//wikihyper.findstart(bw);
				rhyper = wikihyper.find(bw);
			} else {
				//wikihyper.findstart(rred);
				rhyper = wikihyper.find(rred);
			}
			if (rhyper == null) {
				return false;
			} else {
				return (new String(rhyper, "UTF-8")).indexOf(w2) != -1;
			}
		} catch(UnsupportedEncodingException e) {
		}
		return false;
	}
}

package res.ja;

import fuku.eb4j.Book;
import fuku.eb4j.SubBook;
import fuku.eb4j.Searcher;
import fuku.eb4j.Result;
import fuku.eb4j.EBException;
import fuku.eb4j.hook.DefaultHook;
import java.util.ArrayList;

import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.HashMap;
import java.util.HashSet;

public class JaGoiTaikei {
	
	private static String[][] ord = null;
	private static int[][] prop = null;
	private static String[][] pred = null;
	private static HashMap<String, String> stdmap = null;
	//private static HashSet<String> leafcats = null;
	private static boolean initialized = initialize();
	
	private static boolean initialize() {
		boolean ret = false;
		try {
			String imipath = JaGoiTaikei.class.getClassLoader().getResource("data/ja/NihongoGoiTaikei/imitaikei.obj").getPath();
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(imipath));
			ord = (String[][])ois.readObject();
			prop = (int[][])ois.readObject();
			pred = (String[][])ois.readObject();
			ois.close();
			//ObjectInputStream ois2 = new ObjectInputStream(new FileInputStream("../lib/NihongoGoiTaikei/葉カテゴリ.obj"));
			//leafcats = (HashSet<String>)ois2.readObject();
			//ois2.close();
			String jnpath = JaGoiTaikei.class.getClassLoader().getResource("data/ja/JumanNorm/dic.obj").getPath();
			ObjectInputStream ois3 = new ObjectInputStream(new FileInputStream(jnpath));
			stdmap = (HashMap<String, String>)ois3.readObject();
			ois3.close();
			ret = true;
		} catch(Exception e) {
			System.err.println("cannot read imitaikei.obj.");
		}
		return ret;
	}
	
	private static SubBook sbook = getSubBook();
	
	private static SubBook getSubBook() {
		SubBook ret = null;
		try {
			String bkpath = JaGoiTaikei.class.getClassLoader().getResource("data/ja/NihongoGoiTaikei").getPath();
			ret = (new Book(bkpath)).getSubBooks()[0];
		} catch(EBException e) {
			System.err.println("cannot open ebook.");
		}
		return ret;
	}
	/*
	private static class MyHook extends DefaultHook {
		
		MyHook() {
			super(sbook);
		}
		
		@Override
		public void endReference(long pos) {
			System.err.println("has reference: " + pos);
			String text = null;
			try {
				text = sbook.getText(pos, new DefaultHook(sbook));
			} catch(EBException e) {
				System.err.println("error during reading reference.");
			}
			System.out.println(text);
		}
	}*/
	
	private static DefaultHook hook = new DefaultHook(sbook);
	
	private static int getIndex(String s) {
		String cont = s.replaceFirst("[1-9][0-9]*", "");
		return Integer.parseInt(s.substring(0, s.indexOf(cont)));
	}
	
	private static void addGoiEntry(long entryID, String text, ArrayList<JaGoiEntry> list) {
		
		//System.err.println(text);
		
		String[] spn = text.split("[\r\n]+", -1);
		
		String lemma = spn[0].split("[,\\(\\[]", 2)[0];
		
		int i = spn[0].indexOf("[");
		if (i != -1) {
			int j = spn[0].indexOf("]", i + 1);
			String pos = spn[0].substring(i + 1, j);
			if (spn[0].length() > j + 1) {
				assert(spn[0].charAt(j + 1) == '　');
				String[] sps = spn[0].substring(j + 2, spn[0].length()).split("[ 　]+");
				for (int k = 0; k < sps.length; k++) {
					int index = getIndex(sps[k]);
					String[] cats = ord[index];
					if (sps[k].equals(cats[0])) {
						list.add(new JaGoiEntry(entryID, lemma, pos, cats, null, null));
					} else {
						for (int l = 0; l < prop[index].length; l++) {
							list.add(new JaGoiEntry(entryID, lemma, pos, ord[prop[index][l]], null, null));
						}
					}
				}
			} else {
				list.add(new JaGoiEntry(entryID, lemma, pos, null, null, null));
			}
		} else {
			String[] sp2ws = spn[2].split("　", 2);
			String[] spss = sp2ws[0].split(" ", -1);
			String[][] frame = new String[spss.length - 1][];
			String[][] args = new String[frame.length][];
			for (int k = 0; k < frame.length; k++) {
				if (spss[k].charAt(0) == 'N') {
					args[k] = spss[k].substring(2, spss[k].length()).split("/");
					int a = spn[3].indexOf(spss[k].substring(0, 2) + "(");
					int b = a + 3;
					int counter = 0;
					for (; counter >= 0; b++) {
						if (spn[3].charAt(b) == '(') counter++;
						if (spn[3].charAt(b) == ')') counter--;
					}
					frame[k] = spn[3].substring(a + 3, b - 1).split(" ", -1);
					// debug of the original GoiTaikei text:
					for (int l = 0; l < frame[k].length; l++) {
						if (frame[k][l].matches("\\-[0-9].+")) frame[k][l] = frame[k][l].substring(1, frame[k][l].length());
					}
				} else {
					frame[k] = new String[]{spss[k]};
					args[k] = null;
				}
			}
			//boolean thcl = (sp2ws[1].indexOf("（th-cl）") != -1);
			
			int ii = spn[1].indexOf(")　");
			String[] cat = spn[1].substring(ii + 2, spn[1].indexOf("　", ii + 2)).split(" ", -1);
			for (int k = 0; k < cat.length; k++) {
				String[] cats = pred[getIndex(cat[k])];
				assert(cats[0].equals(cat[k]));
				list.add(new JaGoiEntry(entryID, lemma, null, cats, frame, args));
			}
		}
	}
	
	public static JaGoiEntry[] lookup(String word) {
		if (word.isEmpty() || word.matches("[ 　\t]*[0-9０-９・].*")) return new JaGoiEntry[0];
		ArrayList<JaGoiEntry> ret = new ArrayList<JaGoiEntry>();
		
		//System.err.println(word);
		
		try {
			if (stdmap.containsKey(word)) {
				Searcher s1 = sbook.searchExactword(word);
				Searcher s2 = sbook.searchExactword(stdmap.get(word));
				Result res1 = s1.getNextResult();
				Result res2 = s2.getNextResult();
				if (res1 != null && res2 != null) {
					HashSet r1i = new HashSet<Long>();
					do {
						r1i.add(new Long(res1.getTextPosition()));
						res1 = s1.getNextResult();
					} while(res1 != null);
					do {
						Long eid = new Long(res2.getTextPosition());
						if (r1i.contains(eid)) {
							addGoiEntry(eid, res2.getText(hook), ret);
						}
						res2 = s2.getNextResult();
					} while(res2 != null);
				} else {
					while(res2 != null) {
						addGoiEntry(res2.getTextPosition(), res2.getText(hook), ret);
						res2 = s2.getNextResult();
					}
				}
			} else {
				Searcher s = sbook.searchExactword(word);
				Result res = s.getNextResult();
				while (res != null) {
					addGoiEntry(res.getTextPosition(), res.getText(hook), ret);
					res = s.getNextResult();
				}
			}
		} catch(EBException e) {
			System.err.println("error during ebook reading.");
		}
		return ret.toArray(new JaGoiEntry[ret.size()]);
	}
	
	/*
	public static boolean isNonLeaf(GoiEntry entry) {
		if (entry.pos == null || entry.pos.equals("固") || entry.categories == null) {
			return false;
		} else {
			return !leafcats.contains(entry.categories[0]);
		}
	}
	*/
	
	/*
	private static HashSet<Long> getEntryPositions(String word) {
		
		HashSet<Long> ret = new HashSet<Long>();
		
		try {
			Searcher s = sbook.searchExactword(word);
			Result res = s.getNextResult();
			while (res != null) {
				ret.add(new Long(res.getTextPosition()));
				res = s.getNextResult();
			}
			if (stdmap.containsKey(word)) {
				Searcher sstd = sbook.searchExactword(stdmap.get(word));
				Result resstd = sstd.getNextResult();
				while (resstd != null) {
					ret.add(new Long(resstd.getTextPosition()));
					resstd = sstd.getNextResult();
				}
			}
		} catch(EBException e) {
			System.err.println("error during ebook reading.");
		}
		
		return ret;
	}
	
	public static boolean sameEntry(String w1, String w2) {
		if (w1.isEmpty() || w1.substring(0, 1).matches("[0-9０-９]") || w2.isEmpty() || w2.substring(0, 1).matches("[0-9０-９]")) return false;
		HashSet<Long> eps = getEntryPositions(w1);
		eps.retainAll(getEntryPositions(w2));
		return !eps.isEmpty();
	}
	*/
	public static String[] lookupNE(String word) {
		
		if (word.isEmpty() || word.substring(0, 1).matches("[0-9０-９]") || word.matches("[A-Za-z0-9]+")) return new String[0];
		
		boolean allprop = true;
		boolean aux = true;
		ArrayList<String> ret = new ArrayList<String>();
		
		int counter = 0;
		
		try {
			Searcher s = sbook.searchExactword(word);
			Result res = s.getNextResult();
			while (res != null) {
				counter++;
				
				String text = res.getText(hook);
				
				String[] spn = text.split("[\r\n]+", -1);
				int i = spn[0].indexOf("[");
				if (i != -1) {
					
					int j = spn[0].indexOf("]", i + 1);
					String pos = spn[0].substring(i + 1, j);
					if (!pos.equals("接頭") && !pos.equals("接尾") && !pos.equals("固")) aux = false;
					if (pos.equals("固")) {
					
						if (spn[0].length() > j + 1) {
							assert(spn[0].charAt(j + 1) == '　');
							String[] sps = spn[0].substring(j + 2, spn[0].length()).split("[ 　]+");
							
							counter += sps.length - 1;
							for (int k = 0; k < sps.length; k++) {
								ret.add(sps[k]);
							}
						}
					} else {
						allprop = false;
					}
				}
				
				res = s.getNextResult();
			}
		} catch(EBException e) {
			System.err.println("error during ebook reading.");
		}

		if (ret.size() == 0) {
			return new String[0];
		} else if (allprop) {
			return ret.toArray(new String[ret.size()]);
		} else if (!aux && counter < 4) {
			ret.add(null);
			return ret.toArray(new String[ret.size()]);
		} else {
			return new String[0];
		}
	}
	/*
	public static String[] lookupHead(String word) {
		Searcher s = null;
		try {
			s = sbook.searchWord(word);
		} catch(EBException e) {
			System.err.println("error during searching.");
		}
		return readResults(s);
	}
	
	public static String[] lookupTail(String word) {
		Searcher s = null;
		try {
			s = sbook.searchEndword(word);
		} catch(EBException e) {
			System.err.println("error during searching.");
		}
		return readResults(s);
	}*/
}

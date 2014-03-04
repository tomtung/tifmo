package mylib

import com.strangegizmo.cdb.Cdb

import java.io.ByteArrayInputStream
import java.io.ObjectInputStream

package res.en {
	
	object EnTurian10 {
		
		private[this] val dir = EnTurian10.getClass.getClassLoader.getResource("resources/en/WordVectors/Turian10.cdb").getFile
		
		private[this] val cdb = new Cdb(dir)
		
		def lookup(x: String) = {
			val pre = cdb.find(x.getBytes("UTF-8"))
			val tmp = if (pre == null) cdb.find("*UNKNOWN*".getBytes("UTF-8")) else pre
			(new ObjectInputStream(new ByteArrayInputStream(tmp))).readObject().asInstanceOf[Array[Float]]
		}
		
	}
	
}

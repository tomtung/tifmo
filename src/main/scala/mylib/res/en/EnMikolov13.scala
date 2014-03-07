package mylib

import com.strangegizmo.cdb.Cdb

import java.io.ByteArrayInputStream
import java.io.ObjectInputStream

package res.en {
	
	object EnMikolov13 {
		
		private[this] val dir = EnMikolov13.getClass.getClassLoader.getResource("en/WordVectors/Mikolov13.cdb").getFile
		
		private[this] val cdb = new Cdb(dir)
		
		def lookup(x: String) = {
			val pre = cdb.find(x.getBytes("UTF-8"))
			val tmp = if (pre == null) cdb.find("##".getBytes("UTF-8")) else pre
			(new ObjectInputStream(new ByteArrayInputStream(tmp))).readObject().asInstanceOf[Array[Float]]
		}
		
	}
	
}


import java.io.FileReader
import java.io.BufferedReader
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import scala.collection.mutable

val br = new BufferedReader(new FileReader("inqtabs.txt"))

val posi = mutable.Set.empty[String]
val nega = mutable.Set.empty[String]

def getword(s: String) = {
	val tmp = s.indexOf("#")
	if (tmp == -1) s.toLowerCase else s.substring(0, tmp).toLowerCase
}

val cats = br.readLine().split("\t", -1)
assert(cats(2) == "Positiv")
assert(cats(3) == "Negativ")
assert(cats(74) == "Increas")
assert(cats(75) == "Decreas")

def read() {
	val s = br.readLine()
	if (s != null) {
		val sp = s.split("\t", -1)
		if (sp(2) == "Positiv" || sp(74) == "Increas") posi.add(getword(sp(0)))
		if (sp(3) == "Negativ" || sp(75) == "Decreas") nega.add(getword(sp(0)))
		read()
	}
}
read()

val oos = new ObjectOutputStream(new FileOutputStream("inq.obj"))
oos.writeObject(posi.toSet)
oos.writeObject(nega.toSet)
oos.close()

sys.exit(0)


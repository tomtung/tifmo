import sbt._
import sbt.Keys._
import java.io.{File, FileInputStream, BufferedInputStream, ByteArrayOutputStream, ObjectOutputStream}
import org.rauschig.jarchivelib.ArchiverFactory
import org.apache.commons.compress.compressors.CompressorStreamFactory
import com.strangegizmo.cdb.CdbMake

object TifmoBuild extends Build {
  def needToUnpack(archiveFile: File, destinationFile: File) =
    !destinationFile.exists() ||
    archiveFile.lastModified() > archiveFile.lastModified()

  def unpackArchive(archiveFile: File, destinationFile: File)(implicit logger: Logger) {
    if (needToUnpack(archiveFile, destinationFile)) {
      logger.info("Extracting " + destinationFile + " ...")
      destinationFile.getParentFile.mkdirs()
      ArchiverFactory.createArchiver(archiveFile).extract(archiveFile, destinationFile.getParentFile)
    }
  }

  def buildWordVecCbdFromPack(archiveFile: File, destinationFile: File, dimension: Int)(implicit logger: Logger) {
    if (needToUnpack(archiveFile, destinationFile)) {
      logger.info("Building " + destinationFile + " ...")
      
      val cdbmk = new CdbMake()
      destinationFile.getParentFile.mkdirs()
      cdbmk.start(destinationFile.getCanonicalPath)

      val cis =
        new CompressorStreamFactory().createCompressorInputStream(
          new BufferedInputStream(new FileInputStream(archiveFile)))

      for(line <- io.Source.fromInputStream(cis).getLines()) {
        val splitted = line.split(" ")
        val key = splitted(0)
        val value = splitted.tail.map(_.toFloat)
        assert(value.length == dimension)

        val baos = new ByteArrayOutputStream()
        val oos = new ObjectOutputStream(baos)
        oos.writeObject(value)
        oos.close()

        cdbmk.add(key.getBytes("UTF-8"), baos.toByteArray())
      }

      cdbmk.finish()
    }
  }
}

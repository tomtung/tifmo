// ------ Basic configurations ------ 

name := "tifmo"

scalaVersion := "2.10.3"

sbtVersion := "0.13.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models"
).map(_ withSources() withJavadoc())


// ------ Settings related to resource packs ------

val resourcePacked = settingKey[File]("Directory in which packed resources are located.")

resourcePacked := baseDirectory.value / "packed-resources"


// Wordnet Dictionary

val wordnetDict = settingKey[File]("Location of unpacked wordnet dictionary file.")

wordnetDict := (resourceDirectory in Compile).value / "en/dict"

val wordnetDictPack = settingKey[File]("Location of packed wordnet dictionary file.")

wordnetDictPack := resourcePacked.value / "en/wn3.1.dict.tar.gz"


// Turian10 Word Vectors

val turian10WordVec = settingKey[File]("Location of unpacked Turian10 word vector file.")

turian10WordVec := (resourceDirectory in Compile).value / "en/WordVectors/Turian10.cdb"

val turian10WordVecPack = settingKey[File]("Location of packed Turian10 word vector file.")

turian10WordVecPack := resourcePacked.value / "en/WordVectors/Turian10-embeddings-scaled.EMBEDDING_SIZE=50.txt.gz"


// Mikolov13 Word Vectors

val mikolov13WordVec = settingKey[File]("Location of unpacked Mikolov13 word vector file.")

mikolov13WordVec := (resourceDirectory in Compile).value / "en/WordVectors/Mikolov13.cdb"

val mikolov13WordVecPack = settingKey[File]("Location of packed Mikolov13 word vector file.")

mikolov13WordVecPack := resourcePacked.value / "en/WordVectors/Mikolov13-GoogleNews-vectors-negative300.txt.bz2"


// Define the task for unpacking resources

val unpackResources = taskKey[Unit]("Unpack resources.")

unpackResources := {
  implicit val logger = streams.value.log
  // `simpleUnpack` and `buildCbdFromPack` are defined in project/Build.scala
  unpackArchive(wordnetDictPack.value, wordnetDict.value)
  buildWordVecCbdFromPack(turian10WordVecPack.value, turian10WordVec.value, 50)
  buildWordVecCbdFromPack(mikolov13WordVecPack.value, mikolov13WordVec.value, 300)
}

// Prepend the task unpackResources to compile
compile in Compile := {
  val dummy = unpackResources.value
  (compile in Compile).value
}

// Remove unpacked resources when cleanning
clean := {
  import org.apache.commons.io.FileUtils.deleteQuietly
  deleteQuietly(wordnetDict.value)
  deleteQuietly(turian10WordVec.value)
  deleteQuietly(mikolov13WordVec.value)
  clean.value
}
